use std::{error::Error, process, env, fmt::Display, fs::{self, File}, path::Path, io::Write, time::Instant};

use typerd::parse;

#[derive(Debug)]
pub struct ProgramError(pub String);

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for ProgramError {
	fn description(&self) -> &str {
		"program error"
	}
}

macro_rules! error {
	($message:expr) => {
		return Err(Box::new(ProgramError($message.into())))
	};
}

macro_rules! error_on_fail {
	($result:expr, $message:expr, $( $arg:tt ),* ) => {
		match $result {
			Ok(r) => r,
			Err(err) => error!(format!($message, $( $arg, )* err)),
		}
	};
	($result:expr, $message:expr) => {
		match $result {
			Ok(r) => r,
			Err(err) => error!(format!($message, err)),
		}
	};
}

fn bootstrap() -> Result<(), Box<dyn Error>> {
	let mut args = env::args();
	if args.len() < 2 {
		error!("Too few arguments!");
	}
	args.next();

	for filename in args {
		let contents = error_on_fail!(fs::read_to_string(&filename), "Failed to read file {}: {}", filename);

		let now = Instant::now();
		let block = error_on_fail!(parse(&contents), "{}:{}", filename);

		let elapsed = now.elapsed();

		let output_file_path = Path::new(&filename).with_extension("json");
		let output = error_on_fail!(serde_json::to_string_pretty(&block), "Failed to convert {}'s AST to JSON: {}", filename);

		// panic is panic, a bug there in clippy maybe?
		File::create(output_file_path.clone())
			.map(|mut v| v.write_all(output.as_bytes()))
			.unwrap_or_else(|_| panic!("Failed to create output file for {}", filename))
			.unwrap();

		println!("Compiled {} to {} in {:.2?}", filename, output_file_path.to_string_lossy(), elapsed);
	}
	Ok(())
}

fn main() {
	match bootstrap() {
		Ok(()) => {},
		Err(err) => {
			println!("{}", err);
			process::exit(1);
		},
	}
}
