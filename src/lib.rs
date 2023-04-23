use anyhow::{Error, Result};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HuffmanMessage {
    Uncompressed {
        text: String,
        frequency_table: HashMap<char, u32>,
    },
    Compressed {
        text: Vec<u8>,
        frequency_table: Vec<u8>,
        padding_length: usize,
    },
}

impl HuffmanMessage {
    pub fn new(text: &str) -> HuffmanMessage {
        HuffmanMessage::Uncompressed {
            text: text.to_string(),
            frequency_table: Self::frequency_map(text),
        }
    }

    pub fn from_compressed_bytes(text: Vec<u8>, decompression_method: Vec<u8>) -> HuffmanMessage {
        let padding_length = *decompression_method.first().unwrap_or(&0) as usize;
        let frequency_table = decompression_method.get(1..).unwrap_or(&[]).to_vec();
        HuffmanMessage::Compressed {
            text,
            frequency_table,
            padding_length,
        }
    }

    pub fn compress(self) -> Result<HuffmanMessage> {
        match self {
            HuffmanMessage::Uncompressed {
                text,
                frequency_table,
            } => {
                let heap_queue = heap(&frequency_table);
                let codes = char_codes(heap_queue);
                let encoded_text = encode_text(&text, &codes);

                let padding_len = 8 - encoded_text.len() % 8;
                let padded_encoded_text = pad_encoded_text(&encoded_text, padding_len);
                let text = byte_text(&padded_encoded_text)?;
                // The first byte in the bytes we send is the padding length
                // The decompression function extracts this first byte and uses it to remove the padding
                let mut decompression_method = vec![padding_len as u8];
                let frequency = frequency_table_bytes(&frequency_table);
                decompression_method.extend(frequency);
                Ok(HuffmanMessage::from_compressed_bytes(
                    text,
                    decompression_method,
                ))
            }
            HuffmanMessage::Compressed { .. } => Ok(self),
        }
    }

    pub fn decode(self) -> Result<HuffmanMessage> {
        match self {
            HuffmanMessage::Uncompressed { .. } => Ok(self),
            HuffmanMessage::Compressed {
                text,
                frequency_table,
                padding_length,
            } => {
                let frequency_table = decompress_frequency_table(&frequency_table)?;
                let heap_queue = heap(&frequency_table);
                let decode_map = huffman_codebook(heap_queue);
                let bit_string = bit_string(&text);
                let encoded_text = remove_padding(bit_string, padding_length)?;
                Ok(HuffmanMessage::Uncompressed {
                    text: decode_text(encoded_text, &decode_map),
                    frequency_table,
                })
            }
        }
    }

    fn frequency_map(text: &str) -> HashMap<char, u32> {
        let mut freq: HashMap<char, u32> = HashMap::new();
        for c in text.chars() {
            let count = freq.entry(c).or_insert(0);
            *count += 1;
        }
        freq
    }
}

// ----------------- Compression Methods -----------------

/// Converts the padded message into a vector of bytes.
/// The paddes_text is a string of 0s and 1s. We need to convert it into a vector of bytes.
fn byte_text(padded_text: &str) -> Result<Vec<u8>> {
    let mut bytes: Vec<u8> = Vec::new();
    for i in (0..padded_text.len()).step_by(8) {
        let byte_str = byte_vec_helper(padded_text, i)?;
        let byte = u8::from_str_radix(byte_str, 2)?;
        bytes.push(byte);
    }
    Ok(bytes)
}

/// We grab a slice of the text from i to i + 8
/// This can cause an error so we need to handle it
fn byte_vec_helper(text: &'_ str, i: usize) -> Result<&'_ str> {
    let error = Error::msg("Byte sring did not have 8 bits");
    let byte_str = &text.get(i..i + 8).ok_or(error)?;
    Ok(byte_str)
}

/// takes the original message and maps each character to its corresponding Huffman code.
/// The resulting binary string is then used to build the compressed message.
fn encode_text(text: &str, codes: &HashMap<char, String>) -> String {
    let mut encoded_text = String::new();
    for c in text.chars() {
        if let Some(code) = codes.get(&c) {
            encoded_text += code;
        }
    }
    encoded_text
}

fn char_codes(heap_queue: Vec<HeapNode>) -> HashMap<char, String> {
    let (codes, _) = codes_helper(heap_queue);
    codes
}

fn huffman_codebook(heap_queue: Vec<HeapNode>) -> HashMap<String, char> {
    let (_, codebook) = codes_helper(heap_queue);
    codebook
}

/// Maps each character to its corresponding Huffman code.
/// characters with a high frequency will have a lower binary than characters with a low frequency.
fn codes_helper(mut heap_queue: Vec<HeapNode>) -> (HashMap<char, String>, HashMap<String, char>) {
    let mut codes: HashMap<char, String> = HashMap::new();
    let mut codebook: HashMap<String, char> = HashMap::new();
    // The amount of bits needed is equal to the log base 2 of the number of characters in the message.
    let bit_len = f64::ceil(f64::log2(heap_queue.len() as f64)) as usize;
    let mut counter = 0;

    while let Some(node) = heap_queue.pop() {
        let binary_string = format!("{:0width$b}", counter, width = bit_len);
        codes.insert(node.char, binary_string.clone());
        codebook.insert(binary_string, node.char);
        counter += 1;
    }
    (codes, codebook)
}

/// Maps each character to its corresponding Huffman code.
/// characters with a high frequency will have a lower binary than characters with a low frequency.
fn codes_helper2(heap_queue: HashMap<char, u32>) -> (HashMap<char, String>, HashMap<String, char>) {
    // The amount of bits needed is equal to the log base 2 of the number of characters in the message.
    let bit_len = f64::ceil(f64::log2(heap_queue.len() as f64)) as usize;
    let mut queue = heap_queue.into_iter().collect::<Vec<_>>();
    let mut codes: HashMap<char, String> = HashMap::new();
    let mut codebook: HashMap<String, char> = HashMap::new();
    let mut counter = 0;
    while let Some((node, _)) = queue.pop() {
        let binary_string = format!("{:0width$b}", counter, width = bit_len);
        codes.insert(node, binary_string.clone());
        codebook.insert(binary_string, node);
        counter += 1;
    }
    (codes, codebook)
}

/// Turns the frequency map into a min heap.
/// The min heap contains HeapNodes which are used to build the Huffman tree.
/// The HeapNodes are sorted by their frequency.
fn heap(frequency: &HashMap<char, u32>) -> Vec<HeapNode> {
    let mut heap_queue: Vec<HeapNode> = frequency
        .iter()
        .map(|(key, value)| HeapNode::new(*key, *value))
        .collect();
    heap_queue.sort();
    heap_queue
}

/// A byte is 8 bits, the characters with a high frequency will have a short binary string.
/// Therefore, we need to pad the message with 0s so that the message is a multiple of 8 bits.
/// 8 bits can be represented by a single byte.
fn pad_encoded_text(encoded_text: &str, padding_len: usize) -> String {
    let mut padded_text = String::from(encoded_text);
    for _ in 0..padding_len {
        padded_text += "0";
    }
    padded_text
}

// ---------------- Decompression Methods ----------------

// Essentially the same as the compression methods but in reverse...

fn bit_string(text: &[u8]) -> String {
    let mut bit_string = String::new();
    for byte in text {
        let bits = format!("{:08b}", byte); // Convert byte to binary string
        bit_string += &bits;
    }
    bit_string
}

/// The bytes for decompression, the first byte is the amount of padding.
fn frequency_table_bytes(frequency: &HashMap<char, u32>) -> Vec<u8> {
    let mut bytes = Vec::new();

    for (key, value) in frequency.iter() {
        let key = (*key as u32).to_be_bytes();
        bytes.extend(key);
        bytes.extend(value.to_be_bytes());
    }
    bytes
}

pub fn decompress_frequency_table(byte_vec: &[u8]) -> Result<HashMap<char, u32>> {
    let mut map = HashMap::new();
    for byte in byte_vec.chunks(8) {
        let error = Error::msg("Invalid byte vector");
        if byte.len() == 8 {
            let key_u32 = u32::from_be_bytes([byte[0], byte[1], byte[2], byte[3]]);
            let key = char::from_u32(key_u32).ok_or(error)?;
            let value = u32::from_be_bytes([byte[4], byte[5], byte[6], byte[7]]);
            map.insert(key, value);
        } else {
            return Err(error);
        }
    }
    Ok(map)
}

fn decode_text(encoded_text: String, decode_map: &HashMap<String, char>) -> String {
    let mut current_code = String::new();
    let mut decoded_text = String::new();

    for bit in encoded_text.chars() {
        current_code.push(bit);
        if let Some(decoded_char) = decode_map.get(&current_code) {
            decoded_text.push(*decoded_char);
            current_code.clear();
        }
    }
    decoded_text
}

fn remove_padding(encoded_text: String, padding_len: usize) -> Result<String> {
    encoded_text
        .get(..encoded_text.len() - padding_len)
        .map_or_else(
            || Err(Error::msg("Invalid padding length")),
            |s| Ok(s.to_string()),
        )
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct HeapNode {
    char: char,
    freq: u32,
}

impl HeapNode {
    pub fn new(char: char, freq: u32) -> HeapNode {
        HeapNode { char, freq }
    }
}

mod test {
    #[test]
    fn frequency_works() {
        let freq = super::HuffmanMessage::frequency_map("Hello World!");
        assert_eq!(freq.get(&'H').unwrap(), &1);
        assert_eq!(freq.get(&'e').unwrap(), &1);
        assert_eq!(freq.get(&'l').unwrap(), &3);
        assert_eq!(freq.get(&'o').unwrap(), &2);
        assert_eq!(freq.get(&'W').unwrap(), &1);
        assert_eq!(freq.get(&'r').unwrap(), &1);
        assert_eq!(freq.get(&'d').unwrap(), &1);
        assert_eq!(freq.get(&'!').unwrap(), &1);
    }

    #[test]
    fn frequency_works_with_large_text() {
        let freq = super::HuffmanMessage::frequency_map(include_str!("test_files/rick_roll.txt"));
        println!("{:?}", freq.get(&'e').unwrap());
    }

    #[test]
    fn char_codes_work() {
        let freq = super::HuffmanMessage::frequency_map("blub blub blub blub");
        let heap_queue = super::heap(&freq);
        let codes = super::char_codes(heap_queue);
        assert_eq!(codes.len(), 4);
        assert_eq!(codes.get(&'b').unwrap().len(), 2);
    }

    #[test]
    fn char_codes_work_with_large_text() {
        let freq =
            super::HuffmanMessage::frequency_map(include_str!("test_files/bing_chilling.txt"));
        let heap_queue = super::heap(&freq);
        let codes = super::char_codes(heap_queue);
        assert_eq!(codes.len(), 47);
        assert_eq!(codes.get(&'h').unwrap().len(), 6);
    }

    #[test]
    fn huffman_codebook_works() {
        let freq = super::HuffmanMessage::frequency_map("blub blub blub blub");
        let heap_queue = super::heap(&freq);
        let codes = super::char_codes(heap_queue.clone());
        let codebook = super::huffman_codebook(heap_queue);
        assert_eq!(codes.len(), codebook.len());
        for (key, value) in codes.iter() {
            assert_eq!(codebook.get(value).unwrap(), key);
        }
    }

    #[test]
    fn huffman_codebook_works_with_large_text() {
        let freq =
            super::HuffmanMessage::frequency_map(include_str!("test_files/bing_chilling.txt"));
        let heap_queue = super::heap(&freq);
        let codes = super::char_codes(heap_queue.clone());
        let codebook = super::huffman_codebook(heap_queue);
        assert_eq!(codes.len(), codebook.len());
        for (key, value) in codes.iter() {
            assert_eq!(codebook.get(value).unwrap(), key);
        }
    }

    #[test]
    fn encode_works() {
        let freq = super::HuffmanMessage::frequency_map("blub blub blub blub");
        let heap_queue = super::heap(&freq);
        let codes = super::char_codes(heap_queue);
        let encoded = super::encode_text("blub blub blub blub", &codes);
        assert_eq!(encoded, "10010010111001001011100100101110010010");
    }

    #[test]
    fn padding_works() {
        let encoded = "10010010111001001011100100101110010010";
        let padding_len = 8 - encoded.len() % 8;
        let padded = super::pad_encoded_text(encoded, padding_len);
        assert_eq!(padded, "1001001011100100101110010010111001001000");
    }

    #[test]
    fn byte_text_works() {
        let encoded = "1001001011100100101110010010111001001000";
        let byte_vec = super::byte_text(encoded).unwrap();
        assert_eq!(byte_vec.len(), 5);
        assert_eq!(byte_vec[0], 146);
        assert_eq!(byte_vec[1], 228);
        assert_eq!(byte_vec[2], 185);
        assert_eq!(byte_vec[3], 46);
        assert_eq!(byte_vec[4], 72);
    }

    #[test]
    fn frequency_table_bytes_works() {
        let freq = super::HuffmanMessage::frequency_map("blub blub blub blub");
        let table = super::frequency_table_bytes(&freq);
        assert_eq!(table.len(), 32);
        for i in 00..31 {
            if i % 4 == 0 {
                assert_eq!(table[i], 0);
            }
        }
    }

    #[test]
    fn from_bytes_work() {
        let text = "blub blub blub blub";
        let frequency_table = super::HuffmanMessage::frequency_map(text);
        let heap_queue = super::heap(&frequency_table);
        let codes = super::char_codes(heap_queue);
        let encoded_text = super::encode_text(&text, &codes);

        let padding_len = 8 - encoded_text.len() % 8;
        let padded_encoded_text = super::pad_encoded_text(&encoded_text, padding_len);
        let text = super::byte_text(&padded_encoded_text).unwrap();
        let mut decompression_method = vec![padding_len as u8];
        let frequency = super::frequency_table_bytes(&frequency_table);
        decompression_method.extend(frequency);
        let huffman = super::HuffmanMessage::from_compressed_bytes(text, decompression_method);
        if let super::HuffmanMessage::Compressed { text, .. } = huffman {
            println!("{:?}", text);
            assert_eq!(text, [146, 228, 185, 46, 72]);
        } else {
            panic!("HuffmanData is not compressed");
        }
    }

    #[test]
    fn compression_works() {
        let original = include_str!("test_files/rick_roll.txt");
        let text = super::HuffmanMessage::new(original);
        let compressed = text.compress().unwrap();
        let decompressed = compressed.decode().unwrap();
        if let super::HuffmanMessage::Uncompressed { text, .. } = decompressed {
            assert_eq!(text, original);
        }
    }

    #[test]
    fn compression_non_ascii_works() {
        let original = include_str!("test_files/bing_chilling.txt");
        let text = super::HuffmanMessage::new(original);
        let compressed = text.compress().unwrap();
        let decompressed = compressed.decode().unwrap();
        if let super::HuffmanMessage::Uncompressed { text, .. } = decompressed {
            assert_eq!(text, original);
        }
    }

    #[test]
    fn compression_is_smaller() {
        let original = include_str!("test_files/rick_roll.txt");
        let text = super::HuffmanMessage::new(original);
        let compressed = text.compress().unwrap();
        if let super::HuffmanMessage::Compressed { text, .. } = compressed {
            println!(
                "compressed bytes: {}",
                original.bytes().len() as i64 - text.len() as i64
            );
            assert!(text.len() < original.bytes().len());
        }
    }

    #[test]
    fn compression_is_smaller_non_ascii() {
        let original = include_str!("test_files/bing_chilling.txt");
        let text = super::HuffmanMessage::new(original);
        let compressed = text.compress().unwrap();
        if let super::HuffmanMessage::Compressed { text, .. } = compressed {
            println!(
                "compressed bytes: {}",
                original.bytes().len() as i64 - text.len() as i64
            );
            assert!(text.len() < original.bytes().len());
        }
    }
}
