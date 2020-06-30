/*! Commandline application to wait for another process to stop.

 After you started a long running command in one terminal, you decide to want
 to run another command after the first command is finished.

 This is where `after` will help. You simply give it some words of the
 command you started and it will look up the pid and wait for it to exit.

 ## Example

 Open a terminal with `sleep 1m` then open another terminal and
 type `after sleep; echo Done`.

 The `after` command will now wait for the `sleep` command to exit and exit
 itself, allow `echo Done` to be executed.

*/

use clap::{App, Arg};
use libc::pid_t;
use procfs::process::Process;
use std;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process;
use std::{thread, time};

/// Struct to collect information on the process
struct ProcessCommandline {
    process_id: pid_t,
    commandline: String,
}

///Get a list of other pids running on the system
fn other_pids() -> io::Result<Vec<pid_t>> {
    let mut pids: Vec<pid_t> = Vec::new();
    let self_pid = process::id() as pid_t;
    for entry in fs::read_dir("/proc")? {
        let entry = entry?;
        let path: std::path::PathBuf = entry.path();
        if path.is_dir() {
            let dirname: String = path
                .as_path()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string();
            if dirname.chars().all(char::is_numeric) {
                let other_pid = dirname.parse::<pid_t>().unwrap();
                if other_pid != self_pid {
                    pids.push(other_pid)
                }
            }
        }
    }
    Ok(pids)
}

///Get the commandline of the given process (if allowed and succesfull)
fn get_cmdline(pid: &pid_t) -> Option<ProcessCommandline> {
    match Process::new(*pid) {
        Ok(process) => match process.cmdline() {
            Ok(commandline) => Some(ProcessCommandline {
                process_id: *pid,
                commandline: commandline.join(" "),
            }),
            Err(_) => None,
        },

        Err(_) => None,
    }
}

///Given the words to match and the process description, see if the process matches
fn select_pid(match_words: &Vec<String>, process_commandline: &ProcessCommandline) -> bool {
    let sentence = match_words.join(" ");
    process_commandline.commandline.contains(sentence.as_str())
}

///Main entrypoint for the commandline interface
fn main() {
    let matches = App::new("after")
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about("Exit after other commands are done")
        .arg(
            Arg::with_name("PROC")
                .help("Parts of the commandline of the process you want to wait for")
                .multiple(true),
        )
        .get_matches();

    match matches.values_of("PROC") {
        Some(arg_array) => {
            let match_words: Vec<String> = arg_array.map(|v| String::from(v)).collect();

            let wait_list: Vec<ProcessCommandline> = other_pids()
                .unwrap()
                .iter()
                .flat_map(get_cmdline)
                .filter(|pc| select_pid(&match_words, pc))
                .collect();
            for process_commandline in &wait_list {
                println!(
                    "waiting for {}: {}",
                    process_commandline.process_id, process_commandline.commandline
                );
            }
            for process_commandline in &wait_list {
                let mut proc_path = PathBuf::new();
                proc_path.push("/proc");
                proc_path.push(process_commandline.process_id.to_string());
                while proc_path.as_path().exists() {
                    thread::sleep(time::Duration::from_millis(500));
                }
                println!("{} is closed", process_commandline.process_id);
            }
        }
        None => {
            println!("Missing required command description");
            std::process::exit(1);
        }
    }
}
