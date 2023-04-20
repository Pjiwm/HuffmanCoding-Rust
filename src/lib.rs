use std::{cmp::Ordering, collections::HashMap, error::Error};

struct HuffmanCoding<'a> {
    msg: &'a str,
    codes: HashMap<char, String>,
    reverse_codes: HashMap<String, char>,
}

#[allow(dead_code)]
impl<'a> HuffmanCoding<'a> {
    pub fn new(msg: &'a str) -> HuffmanCoding<'a> {
        let mut huff = HuffmanCoding {
            msg,
            codes: HashMap::new(),
            reverse_codes: HashMap::new(),
        };
        let codes = Self::make_codes(&huff);
        huff.codes = codes.0;
        huff.reverse_codes = codes.1;
        huff
    }

    // ----------------- Compression Methods -----------------

    /// Compresses the message as a vector of bytes
    pub fn compress(&self) -> Result<Vec<u8>, Box<dyn Error>> {
        let encoded_msg = self.encoded_msg();
        let padded_encoded_msg = self.pad_encoded_msg(encoded_msg);
        self.byte_vec(padded_encoded_msg)
    }

    /// Converts the padded message into a vector of bytes.
    /// The paddes_msg is a string of 0s and 1s. We need to convert it into a vector of bytes.
    fn byte_vec(&self, padded_msg: String) -> Result<Vec<u8>, Box<dyn Error>> {
        let mut bytes: Vec<u8> = Vec::new();
        for i in (0..padded_msg.len()).step_by(8) {
            let byte_str = self.byte_vec_helper(&padded_msg, i)?;
            let byte = u8::from_str_radix(byte_str, 2)?;
            bytes.push(byte);
        }
        Ok(bytes)
    }

    /// We grab a slice of the msg from i to i + 8
    /// This can cause an error so we need to handle it
    fn byte_vec_helper<'b>(&self, msg: &'b str, i: usize) -> Result<&'b str, std::io::Error> {
        let error = std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Byte sring did not have 8 bits",
        );
        let byte_str = &msg.get(i..i + 8).ok_or(error)?;
        Ok(byte_str)
    }

    /// takes the original message and maps each character to its corresponding Huffman code.
    /// The resulting binary string is then used to build the compressed message.
    fn encoded_msg(&self) -> String {
        let mut encoded_msg = String::new();
        for c in self.msg.chars() {
            if let Some(code) = self.codes.get(&c) {
                encoded_msg += code;
            }
        }
        encoded_msg
    }
    /// Maps each character to its corresponding Huffman code.
    /// characters with a high frequency will have a lower binary than characters with a low frequency.
    fn make_codes(&self) -> (HashMap<char, String>, HashMap<String, char>) {
        let mut heap_queue = self.make_heap();
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

    /// Helper function for incrementing the binary counter correctly.
    /// 000 -> 001 -> 010 -> 011 -> 100 etc.
    fn increment_binary_counter(&self, counter: &mut Vec<bool>) {
        if let Some(i) = counter.iter().rposition(|&b| !b) {
            counter[i] = true;
            for j in i + 1..counter.len() {
                counter[j] = false;
            }
        } else {
            counter.push(true);
        }
    }

    /// Calculates the frequency of each character in the message
    /// and stores it in a HashMap
    fn make_frequency_map(&self) -> HashMap<char, u32> {
        let mut freq: HashMap<char, u32> = HashMap::new();
        for c in self.msg.chars() {
            let count = freq.entry(c).or_insert(0);
            *count += 1;
        }
        freq
    }

    /// Turns the frequency map into a min heap.
    /// The min heap contains HeapNodes which are used to build the Huffman tree.
    /// The HeapNodes are sorted by their frequency.
    fn make_heap(&self) -> Vec<HeapNode> {
        let frequency = self.make_frequency_map();
        let mut heap_queue: Vec<HeapNode> = frequency
            .iter()
            .map(|(key, value)| HeapNode::new(*key, *value))
            .collect();
        heap_queue.sort_by(|a, b| a.freq.cmp(&b.freq));
        heap_queue
    }

    /// A byte is 8 bits, the characters with a high frequency will have a short binary string.
    /// Therefore, we need to pad the message with 0s so that the message is a multiple of 8 bits.
    /// 8 bits can be represented by a single byte.
    fn pad_encoded_msg(&self, mut encoded_msg: String) -> String {
        let extra_padding = 8 - encoded_msg.len() % 8;
        for _ in 0..extra_padding {
            encoded_msg += "0";
        }
        // Format into binary string
        let padded_info = format!("{:08b}", extra_padding);
        padded_info + &encoded_msg
    }

    // ---------------- Deompression Methods ----------------

    // Essentially the same as the compression methods but in reverse...

    pub fn decompress(
        bytes: Vec<u8>,
        decode_map: HashMap<String, char>,
    ) -> Result<String, Box<dyn Error>> {
        let mut bit_string = String::new();
        for byte in bytes {
            let bits = format!("{:08b}", byte); // Convert byte to binary string
            bit_string += &bits;
        }

        let encoded_msg = Self::remove_padding(bit_string)?;
        Ok(Self::decode_text(encoded_msg, decode_map))
    }

    fn decode_text(encoded_msg: String, decode_map: HashMap<String, char>) -> String {
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

    fn remove_padding(encoded_msg: String) -> Result<String, Box<dyn Error>> {
        if encoded_msg.len() < 8 {
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Encoded message is too short",
            )));
        }

        let padded_info = &encoded_msg[..8];
        let extra_padding = match usize::from_str_radix(padded_info, 2) {
            Ok(padding) => padding,
            Err(e) => return Err(Box::new(e)),
        };

        let encoded_text = &encoded_msg[8..];
        if let Some(unpadded_text) = encoded_text.get(..encoded_text.len() - extra_padding) {
            Ok(unpadded_text.to_string())
        } else {
            Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Invalid encoded message",
            )))
        }
    }
}
#[derive(Debug)]
struct HeapNode {
    char: char,
    freq: u32,
}

impl HeapNode {
    pub fn new(char: char, freq: u32) -> HeapNode {
        HeapNode { char, freq }
    }
}

impl Ord for HeapNode {
    fn cmp(&self, other: &Self) -> Ordering {
        self.freq.cmp(&other.freq)
    }
}

impl PartialOrd for HeapNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for HeapNode {
    fn eq(&self, other: &Self) -> bool {
        self.freq == other.freq
    }
}

impl Eq for HeapNode {}

mod tests {
    #[test]
    fn make_frequency_map_works() {
        let huff = super::HuffmanCoding::new("Hello World");
        let freq = huff.make_frequency_map();
        assert_eq!(freq.len(), 8);
        assert_eq!(freq.get(&'H'), Some(&1));
        assert_eq!(freq.get(&'e'), Some(&1));
        assert_eq!(freq.get(&'W'), Some(&1));
        assert_eq!(freq.get(&'r'), Some(&1));
        assert_eq!(freq.get(&'l'), Some(&3));
        assert_eq!(freq.get(&'o'), Some(&2));
    }

    #[test]
    fn make_heap_works() {
        let huff = super::HuffmanCoding::new("Hello world");
        let heap = huff.make_heap();
        assert_eq!(heap.len(), 8);
        assert_eq!(heap[7].char, 'l');
        assert_eq!(heap[6].char, 'o');
    }

    #[test]
    fn compress_works() {
        let message = include_str!("test_files/bing_chilling.txt");
        let huff = super::HuffmanCoding::new(message);
        let comp_msg = huff.compress().unwrap();
        let decompressed = super::HuffmanCoding::decompress(comp_msg, huff.reverse_codes).unwrap();
        assert_eq!(decompressed, message);
    }

    #[test]
    fn compression_is_smaller() {
        let message = include_str!("test_files/rick_roll.txt");
        let huff = super::HuffmanCoding::new(message).compress();
        let comp_msg = huff.unwrap();
        assert!(comp_msg.len() < message.bytes().len());
    }
}