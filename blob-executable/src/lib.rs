use std::{
    fs::{self, File},
    io::{Read, Write},
};

use blob_common::{error, info, warn, BUILD_FILE_PATH, VERSION_MAJOR, VERSION_MINOR};

pub const BLOB_EXECUTABLE_FILE_EXTENTION: &'static str = "blobexec";

const HEADER_SIZE: usize = 64;
const HEADER_PADDING: usize = HEADER_SIZE - 6;
const MAGIC_NUMBER: [u8; 4] = [43, 67, 88, 145];
const DATA_OFFSET_N_BYTES: usize = 8;
const PROGRAM_OFFSET_N_BYTES: usize = 8;
const TOTAL_OFFSET_BYTES: usize =
    DATA_OFFSET_N_BYTES + PROGRAM_OFFSET_N_BYTES;

#[repr(C)]
#[derive(Debug)]
pub struct BlobExecutable {
    header: BlobExecutableHeader,
    jump_table: Vec<u64>,
    data: Vec<u8>,
    program: Vec<u8>,
}

impl BlobExecutable {
    pub fn new(jump_table: Vec<u64>, data: Vec<u8>, program: Vec<u8>) -> BlobExecutable {
        BlobExecutable {
            header: BlobExecutableHeader::new(),
            jump_table,
            data,
            program,
        }
    }

    pub fn load_from_file(file_name: &str) -> Result<BlobExecutable, ()> {
        let file = File::open(file_name);
        if file.is_err() {
            error!("Failed to open blob executable file '{file_name}'");
            return Err(());
        }
        let mut file = file.unwrap();
        let mut header_bytes = [0u8; HEADER_SIZE];
        if file.read_exact(&mut header_bytes).is_err() {
            error!("Failed to read header bytes from file '{file_name}'");
            return Err(());
        }

        // Deserialize header section
        let header = BlobExecutableHeader::from_bytes(&header_bytes);
        if !header.is_magic_number_valid() {
            error!("Failed to validate executable format");
            return Err(());
        }

        // Deserialize data offset
        let mut data_offset_bytes = [0u8; DATA_OFFSET_N_BYTES];
        if file.read_exact(&mut data_offset_bytes).is_err() {
            error!("Failed to read data offset");
            return Err(());
        }
        let data_offset = u64::from_be_bytes(data_offset_bytes);

        // Deserialize program offset
        let mut program_offset_bytes = [0u8; PROGRAM_OFFSET_N_BYTES];
        if file.read_exact(&mut program_offset_bytes).is_err() {
            error!("Failed to read program offset");
            return Err(());
        }
        let program_offset = u64::from_be_bytes(program_offset_bytes);

        // Deserialize jump table
        let jump_table_size = data_offset as usize
            - (HEADER_SIZE + TOTAL_OFFSET_BYTES);
        assert!(jump_table_size % 8 == 0);
        let jump_table: Vec<u64>;
        if jump_table_size != 0 {
            let mut jump_table_bytes: Vec<u8> = Vec::with_capacity(jump_table_size);
            if file.read_exact(&mut jump_table_bytes).is_err() {
                error!("Failed to read jump table for file '{file_name}'");
                return Err(());
            }

            jump_table = jump_table_bytes
                .chunks(8)
                .map(|chunk| {
                    let mut arr = [0u8; 8];
                    arr.copy_from_slice(chunk);
                    u64::from_be_bytes(arr)
                })
                .collect();
        } else {
            jump_table = vec![];
        }

        // Deserialize data section
        let data_section_size = (program_offset - data_offset) as usize;
        let mut data: Vec<u8>;
        if data_section_size != 0 {
            data = Vec::with_capacity(data_section_size);
            if file.read_exact(&mut data).is_err() {
                error!("Failed to read data section for file '{file_name}'");
                return Err(());
            }
        } else {
            data = vec![];
        }

        // Deserialize program section
        let program_size;
        if let Ok(metadata) = file.metadata() {
            program_size = metadata.len() - program_offset;
        } else {
            warn!("Failed to read file '{file_name}' metadata");
            program_size = 16384;
        }
        let mut program = Vec::with_capacity(program_size as usize);
        if file.read_to_end(&mut program).is_err() {
            error!("Failed to read program section for file '{file_name}'");
            return Err(());
        }

        Ok(BlobExecutable::new(jump_table, data, program))
    }

    pub fn save(&self, file_name: &str) -> Result<(), ()> {
        if fs::create_dir_all(BUILD_FILE_PATH).is_err() {
            error!("Failed to create build directory");
            return Err(());
        }

        let mut bytes: Vec<u8> = Vec::with_capacity(
            HEADER_SIZE
                + PROGRAM_OFFSET_N_BYTES
                + self.program.len() * 2,
        );

        // Serialize header
        bytes.extend_from_slice(&self.header.to_bytes());

        // Make space for proram offset
        let data_offset_pos = bytes.len();
        bytes.extend_from_slice(&[0u8; DATA_OFFSET_N_BYTES]);
        let program_offset_pos = bytes.len();
        bytes.extend_from_slice(&[0u8; PROGRAM_OFFSET_N_BYTES]);

        // Serialize jump table
        bytes.extend_from_slice(
            &self
                .jump_table
                .iter()
                .flat_map(|entry| entry.to_be_bytes())
                .collect::<Vec<u8>>(),
        );

        // Populate data offset
        let data_offset = bytes.len();
        let data_offset_bytes = (data_offset as u64).to_be_bytes();
        bytes[data_offset_pos..data_offset_pos + DATA_OFFSET_N_BYTES]
            .copy_from_slice(&data_offset_bytes);

        // Serialize data section
        bytes.extend_from_slice(&self.data);

        // Populate program offset
        let program_offset = bytes.len();
        let program_offset_bytes = (program_offset as u64).to_be_bytes();
        bytes[program_offset_pos..program_offset_pos + PROGRAM_OFFSET_N_BYTES]
            .copy_from_slice(&program_offset_bytes);

        // Serialize program
        bytes.extend_from_slice(&self.program);

        // Save
        let file = File::create(file_name);
        if file.is_err() {
            error!("Failed to create file '{file_name}'");
            return Err(());
        }
        let mut file = file.unwrap();
        match file.write_all(&bytes) {
            Ok(_) => {
                info!("Successfully saved blob executable file '{file_name}'");
                Ok(())
            }
            Err(_) => {
                error!("Failed to create blob executable file '{file_name}'");
                Err(())
            }
        }
    }

    pub fn get_jump_table(&self) -> Vec<usize> {
        self.jump_table
            .iter()
            .map(|entry| *entry as usize)
            .collect()
    }

    pub fn get_program(&self) -> &Vec<u8> {
        &self.program
    }

    pub fn get_global_data(&self) -> &Vec<u8> {
        &self.data
    }
}

#[repr(C)]
#[derive(Debug)]
struct BlobExecutableHeader {
    magic_number: [u8; 4],
    version_major: u8,
    version_minor: u8,
    _padding: [u8; HEADER_PADDING],
}

impl BlobExecutableHeader {
    pub fn new() -> BlobExecutableHeader {
        BlobExecutableHeader {
            magic_number: MAGIC_NUMBER,
            version_major: VERSION_MAJOR,
            version_minor: VERSION_MINOR,
            _padding: [0; HEADER_PADDING],
        }
    }

    pub fn from_bytes(bytes: &[u8; HEADER_SIZE]) -> BlobExecutableHeader {
        unsafe {
            std::mem::transmute::<[u8; HEADER_SIZE], BlobExecutableHeader>(*bytes)
        }
    }

    fn is_magic_number_valid(&self) -> bool {
        self.magic_number.eq(&MAGIC_NUMBER)
    }

    fn to_bytes(&self) -> [u8; 64] {
        let mut bytes = [0u8; 64];
        bytes[0..4].copy_from_slice(&self.magic_number);
        bytes[4] = self.version_major;
        bytes[5] = self.version_minor;
        bytes[6..64].copy_from_slice(&self._padding);
        bytes
    }
}
