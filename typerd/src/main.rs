use std::{
	env,
	error::Error,
	fmt::Display,
	fs::{self, File},
	io::Write,
	path::Path,
	process,
	thread::{self, JoinHandle},
	time::Instant,
};

use typerd_parser::parse;

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
		return Err(format!("{}", ProgramError($message.into())))
	};
	($message:expr, $( $arg:tt ),*) => {
		return Err(format!("{}", ProgramError($message.into()), $( $arg, )*))
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

type ProgramResult = Result<(), String>;

fn compile(file_name: String) -> ProgramResult {
	let contents = error_on_fail!(
		fs::read_to_string(&file_name),
		"Failed to read file {}: {}",
		file_name
	);

	let now = Instant::now();
	let block = error_on_fail!(parse(&contents), "{}:{}", file_name);
	let elapsed = now.elapsed();

	println!("Compiled {} in {:.2?}", file_name, elapsed);

	// let output_file_path = Path::new(&file_name).with_extension("json");
	// let output = error_on_fail!(
	// 	serde_json::to_string_pretty(&block),
	// 	"Failed to convert {}'s AST to JSON: {}",
	// 	file_name
	// );

	use typerd_checker::ModuleInfo;

	let now = Instant::now();
	let module = error_on_fail!(ModuleInfo::new(&block), "{}:{}", file_name);
	let elapsed = now.elapsed();

	println!("Typechecked {} in {:.2?}", file_name, elapsed);

	// panic is panic, a bug there in clippy maybe?
	// File::create(output_file_path.clone())
	// 	.map(|mut v| v.write_all(output.as_bytes()))
	// 	.unwrap_or_else(|_| panic!("Failed to create output file for {}", file_name))
	// 	.unwrap();

	println!("{:#?}", module.analyzer);

	Ok(())
}

fn bootstrap() -> Result<(), String> {
	let mut args = env::args();
	if args.len() < 2 {
		error!("Too few arguments!");
	}
	args.next();

	let mut handlers: Vec<JoinHandle<ProgramResult>> = Vec::new();
	for file_name in args {
		let handle = thread::spawn(|| compile(file_name));
		handlers.push(handle);
	}

	let mut terminate = false;
	for handle in handlers {
		let res = handle
			.join()
			.map_err(|v| format!("Failed to wait for a thread: {:#?}", v))?;
		if !terminate {
			match res {
				Ok(_) => continue,
				Err(err) => {
					println!("{}", err);
					println!("Terminating compiler process because of this error.");
					terminate = true;
				},
			};
		}
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
