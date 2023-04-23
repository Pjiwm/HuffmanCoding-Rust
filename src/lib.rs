use std::{collections::HashMap, error::Error};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum HuffmanData {
    Decompressed {
        msg: String,
        frequency_table: HashMap<char, u32>,
    },
    Compressed {
        msg: Vec<u8>,
        frequency_table: Vec<u8>,
        padding_length: usize,
    },
}

impl HuffmanData {
    pub fn new(msg: &str) -> HuffmanData {
        HuffmanData::Decompressed {
            msg: msg.to_string(),
            frequency_table: Self::frequency_map(msg),
        }
    }

    pub fn from_bytes(msg: Vec<u8>, decompression_method: Vec<u8>) -> HuffmanData {
        let padding_length = *decompression_method.first().unwrap_or(&0) as usize;
        let frequency_table = decompression_method.get(1..).unwrap_or(&[]).to_vec();
        HuffmanData::Compressed {
            msg,
            frequency_table,
            padding_length,
        }
    }

    fn frequency_map(msg: &str) -> HashMap<char, u32> {
        let mut freq: HashMap<char, u32> = HashMap::new();
        for c in msg.chars() {
            let count = freq.entry(c).or_insert(0);
            *count += 1;
        }
        freq
    }
}

// ----------------- Compression Methods -----------------

/// Compresses the message as a vector of bytes
pub fn compress(huffman: HuffmanData) -> Result<HuffmanData, Box<dyn Error>> {
    match huffman {
        HuffmanData::Decompressed {
            msg,
            frequency_table,
        } => {
            let heap_queue = heap(&frequency_table);
            let codes = codes(heap_queue);
            let encoded_msg = encode_msg(&msg, &codes);

            let padding_len = 8 - encoded_msg.len() % 8;
            let padded_encoded_msg = pad_encoded_msg(&encoded_msg, padding_len);
            let msg = byte_msg(&padded_encoded_msg)?;
            // The first byte in the bytes we send is the padding length
            // The decompression function extracts this first byte and uses it to remove the padding
            let mut decompression_method = vec![padding_len as u8];
            let frequency = frequency_table_bytes(frequency_table);
            decompression_method.extend(frequency);
            Ok(HuffmanData::from_bytes(msg, decompression_method))
        }
        HuffmanData::Compressed {
            msg,
            frequency_table,
            padding_length,
        } => Ok(HuffmanData::Compressed {
            msg,
            frequency_table,
            padding_length,
        }),
    }
}

/// Converts the padded message into a vector of bytes.
/// The paddes_msg is a string of 0s and 1s. We need to convert it into a vector of bytes.
fn byte_msg(padded_msg: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut bytes: Vec<u8> = Vec::new();
    for i in (0..padded_msg.len()).step_by(8) {
        let byte_str = byte_vec_helper(padded_msg, i)?;
        let byte = u8::from_str_radix(byte_str, 2)?;
        bytes.push(byte);
    }
    Ok(bytes)
}

/// We grab a slice of the msg from i to i + 8
/// This can cause an error so we need to handle it
fn byte_vec_helper(msg: &'_ str, i: usize) -> Result<&'_ str, std::io::Error> {
    let error = std::io::Error::new(
        std::io::ErrorKind::InvalidData,
        "Byte sring did not have 8 bits",
    );
    let byte_str = &msg.get(i..i + 8).ok_or(error)?;
    Ok(byte_str)
}

/// takes the original message and maps each character to its corresponding Huffman code.
/// The resulting binary string is then used to build the compressed message.
fn encode_msg(msg: &str, codes: &HashMap<char, String>) -> String {
    let mut encoded_msg = String::new();
    for c in msg.chars() {
        if let Some(code) = codes.get(&c) {
            encoded_msg += code;
        }
    }
    encoded_msg
}

fn codes(heap_queue: Vec<HeapNode>) -> HashMap<char, String> {
    let (codes, _) = codes_helper(heap_queue);
    codes
}

fn codes_reverse(heap_queue: Vec<HeapNode>) -> HashMap<String, char> {
    let (_, reverse_map) = codes_helper(heap_queue);
    reverse_map
}

/// Maps each character to its corresponding Huffman code.
/// characters with a high frequency will have a lower binary than characters with a low frequency.
fn codes_helper(mut heap_queue: Vec<HeapNode>) -> (HashMap<char, String>, HashMap<String, char>) {
    let mut codes: HashMap<char, String> = HashMap::new();
    let mut reverse_map: HashMap<String, char> = HashMap::new();
    // The amount of bits needed is equal to the log base 2 of the number of characters in the message.
    let bit_len = f64::ceil(f64::log2(heap_queue.len() as f64)) as usize;
    let mut counter = 0;

    while let Some(node) = heap_queue.pop() {
        let binary_string = format!("{:0width$b}", counter, width = bit_len);
        codes.insert(node.char, binary_string.clone());
        reverse_map.insert(binary_string, node.char);
        counter += 1;
    }
    (codes, reverse_map)
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
fn pad_encoded_msg(encoded_msg: &str, padding_len: usize) -> String {
    let mut padded_msg = String::from(encoded_msg);
    for _ in 0..padding_len {
        padded_msg += "0";
    }
    padded_msg
}

// ---------------- Decompression Methods ----------------

// Essentially the same as the compression methods but in reverse...

pub fn decompress(huffman: HuffmanData) -> Result<HuffmanData, Box<dyn Error>> {
    match huffman {
        HuffmanData::Decompressed {
            msg,
            frequency_table,
        } => Ok(HuffmanData::Decompressed {
            msg,
            frequency_table,
        }),
        HuffmanData::Compressed {
            msg,
            frequency_table,
            padding_length,
        } => {
            let frequency_table = decompress_frequency_table(&frequency_table)?;
            let heap_queue = heap(&frequency_table);
            let decode_map = codes_reverse(heap_queue);
            let bit_string = bit_string(&msg);
            let encoded_msg = remove_padding(bit_string, padding_length)?;
            Ok(HuffmanData::Decompressed {
                msg: decode_text(encoded_msg, &decode_map),
                frequency_table,
            })
        }
    }
}

fn bit_string(msg: &[u8]) -> String {
    let mut bit_string = String::new();
    for byte in msg {
        let bits = format!("{:08b}", byte); // Convert byte to binary string
        bit_string += &bits;
    }
    bit_string
}

/// The bytes for decompression, the first byte is the amount of padding.
fn frequency_table_bytes(frequency: HashMap<char, u32>) -> Vec<u8> {
    let mut bytes = Vec::new();

    for (key, value) in frequency.iter() {
        let key = (*key as u32).to_be_bytes();
        bytes.extend(key);
        bytes.extend(value.to_be_bytes());
    }
    bytes
}

pub fn decompress_frequency_table(byte_vec: &[u8]) -> Result<HashMap<char, u32>, std::io::Error> {
    let mut map = HashMap::new();
    for byte in byte_vec.chunks(8) {
        let error = std::io::Error::new(std::io::ErrorKind::InvalidData, "Invalid byte vector");
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

fn decode_text(encoded_msg: String, decode_map: &HashMap<String, char>) -> String {
    let mut current_code = String::new();
    let mut decoded_text = String::new();

    for bit in encoded_msg.chars() {
        current_code.push(bit);
        if let Some(decoded_char) = decode_map.get(&current_code) {
            decoded_text.push(*decoded_char);
            current_code.clear();
        }
    }
    decoded_text
}

fn remove_padding(encoded_msg: String, padding_len: usize) -> Result<String, std::io::Error> {
    Ok(encoded_msg[..encoded_msg.len() - padding_len].to_string())
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
        let freq = super::HuffmanData::frequency_map("Hello World!");
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
        let freq = super::HuffmanData::frequency_map(include_str!("test_files/rick_roll.txt"));
        println!("{:?}", freq.get(&'e').unwrap());
    }

    #[test]
    fn codes_work() {
        let freq = super::HuffmanData::frequency_map("blub blub blub blub");
        let heap_queue = super::heap(&freq);
        let codes = super::codes(heap_queue);
        assert_eq!(codes.len(), 4);
        assert_eq!(codes.get(&'b').unwrap().len(), 2);
    }

    #[test]
    fn codes_work_with_large_text() {
        let freq = super::HuffmanData::frequency_map(include_str!("test_files/bing_chilling.txt"));
        let heap_queue = super::heap(&freq);
        let codes = super::codes(heap_queue);
        assert_eq!(codes.len(), 47);
        assert_eq!(codes.get(&'h').unwrap().len(), 6);
    }

    #[test]
    fn codes_reverse_works() {
        let freq = super::HuffmanData::frequency_map("blub blub blub blub");
        let heap_queue = super::heap(&freq);
        let codes = super::codes(heap_queue.clone());
        let codes_reverse = super::codes_reverse(heap_queue);
        assert_eq!(codes.len(), codes_reverse.len());
        for (key, value) in codes.iter() {
            assert_eq!(codes_reverse.get(value).unwrap(), key);
        }
    }

    #[test]
    fn codes_reverse_works_with_large_text() {
        let freq = super::HuffmanData::frequency_map(include_str!("test_files/bing_chilling.txt"));
        let heap_queue = super::heap(&freq);
        let codes = super::codes(heap_queue.clone());
        let codes_reverse = super::codes_reverse(heap_queue);
        assert_eq!(codes.len(), codes_reverse.len());
        for (key, value) in codes.iter() {
            assert_eq!(codes_reverse.get(value).unwrap(), key);
        }
    }

    #[test]
    fn encode_works() {
        let freq = super::HuffmanData::frequency_map("blub blub blub blub");
        let heap_queue = super::heap(&freq);
        let codes = super::codes(heap_queue);
        let encoded = super::encode_msg("blub blub blub blub", &codes);
        assert_eq!(encoded, "10010010111001001011100100101110010010");
    }

    #[test]
    fn padding_works() {
        let encoded = "10010010111001001011100100101110010010";
        let padding_len = 8 - encoded.len() % 8;
        let padded = super::pad_encoded_msg(encoded, padding_len);
        assert_eq!(padded, "1001001011100100101110010010111001001000");
    }

    #[test]
    fn byte_msg_works() {
        let encoded = "1001001011100100101110010010111001001000";
        let byte_vec = super::byte_msg(encoded).unwrap();
        assert_eq!(byte_vec.len(), 5);
        assert_eq!(byte_vec[0], 146);
        assert_eq!(byte_vec[1], 228);
        assert_eq!(byte_vec[2], 185);
        assert_eq!(byte_vec[3], 46);
        assert_eq!(byte_vec[4], 72);
    }

    #[test]
    fn frequency_table_bytes_works() {
        let freq = super::HuffmanData::frequency_map("blub blub blub blub");
        let table = super::frequency_table_bytes(freq);
        assert_eq!(table.len(), 32);
        for i in 00..31 {
            if i % 4 == 0 {
                assert_eq!(table[i], 0);
            }
        }
    }

    #[test]
    fn from_bytes_work() {
        let msg = "blub blub blub blub";
        let frequency_table = super::HuffmanData::frequency_map(msg);
        let heap_queue = super::heap(&frequency_table);
        let codes = super::codes(heap_queue);
        let encoded_msg = super::encode_msg(&msg, &codes);

        let padding_len = 8 - encoded_msg.len() % 8;
        let padded_encoded_msg = super::pad_encoded_msg(&encoded_msg, padding_len);
        let msg = super::byte_msg(&padded_encoded_msg).unwrap();
        let mut decompression_method = vec![padding_len as u8];
        let frequency = super::frequency_table_bytes(frequency_table);
        decompression_method.extend(frequency);
        let huffman = super::HuffmanData::from_bytes(msg, decompression_method);
        if let super::HuffmanData::Compressed { msg, .. } = huffman {
            println!("{:?}", msg);
            assert_eq!(msg, [146, 228, 185, 46, 72]);
        } else {
            panic!("HuffmanData is not compressed");
        }
    }

    #[test]
    fn compression_works() {
        let original = include_str!("test_files/rick_roll.txt");
        let msg = super::HuffmanData::new(original);
        let huffman = super::compress(msg).unwrap();
        let decompressed = super::decompress(huffman).unwrap();
        if let super::HuffmanData::Decompressed { msg, .. } = decompressed {
            assert_eq!(msg, original);
        }
    }

    #[test]
    fn compression_non_ascii_works() {
        let original = include_str!("test_files/bing_chilling.txt");
        let msg = super::HuffmanData::new(original);
        let huffman = super::compress(msg).unwrap();
        let decompressed = super::decompress(huffman).unwrap();
        if let super::HuffmanData::Decompressed { msg, .. } = decompressed {
            assert_eq!(msg, original);
        }
    }

    #[test]
    fn compression_is_smaller() {
        let original = include_str!("test_files/rick_roll.txt");
        let msg = super::HuffmanData::new(original);
        let huffman = super::compress(msg).unwrap();
        if let super::HuffmanData::Compressed { msg, .. } = huffman {
            println!(
                "compressed bytes: {}",
                original.bytes().len() as i64 - msg.len() as i64
            );
            assert!(msg.len() < original.bytes().len());
        }
    }


    #[test]
    fn compression_is_smaller_non_ascii() {
        let original = include_str!("test_files/bing_chilling.txt");
        let msg = super::HuffmanData::new(original);
        let huffman = super::compress(msg).unwrap();
        if let super::HuffmanData::Compressed { msg, .. } = huffman {
            println!(
                "compressed bytes: {}",
                original.bytes().len() as i64 - msg.len() as i64
            );
            assert!(msg.len() < original.bytes().len());
        }
    }
}
