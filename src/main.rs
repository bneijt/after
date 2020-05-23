use clap::{App, Arg};
use libc::pid_t;
use procfs::process::Process;
use std;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::{thread, time};
use std::process;

struct ProcessCommandline {
    process_id: pid_t,
    commandline: String,
}

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

fn select_pid(match_words: &Vec<String>, process_commandline: &ProcessCommandline) -> bool {
    let sentence = match_words.join(" ");
    process_commandline.commandline.contains(sentence.as_str())
}

fn main() {
    let matches = App::new("after")
        .version("0.1.0")
        .author("Bram <bram@neijt.nl>")
        .about("Exit after other commands are done")
        .arg(
            Arg::with_name("PROC")
                .help("Process description to wait for")
                .multiple(true),
        )
        .get_matches();

    println!("{:?}", matches);
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
                println!("wait for: {}", process_commandline.commandline);
            }
            for process_commandline in &wait_list {
                let mut proc_path = PathBuf::new();
                proc_path.push("/proc");
                proc_path.push(process_commandline.process_id.to_string());
                while proc_path.as_path().exists() {
                    thread::sleep(time::Duration::from_millis(500));
                }
                println!("after: {}", process_commandline.commandline);
            }
        }
        None => {
            println!("No process description given, try --help maybe?");
            std::process::exit(1);
        }
    }
}
