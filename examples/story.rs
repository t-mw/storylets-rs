use colored::*;

use std::io::{self, Write};
use std::str::FromStr;

fn main() {
    start_loop().unwrap();
}

fn start_loop() -> io::Result<()> {
    let mut context = storylets::Context::from_text(include_str!("story.throne"));

    loop {
        let cards = context.draw_cards();

        let mut stdout = io::stdout();
        for (i, c) in cards.iter().enumerate() {
            print_hr(&mut stdout)?;
            writeln!(stdout, "{}: {}", i.to_string().bold(), c.title.italic())?;
            writeln!(stdout, "   {}", c.description)?;
        }
        print_hr(&mut stdout)?;
        write!(
            stdout,
            "Select a card {} or {} to quit: ",
            format!("(0-{})", cards.len() - 1).bold(),
            "q".bold()
        )?;
        stdout.flush()?;

        let n = match get_input(cards.len() - 1, false)? {
            Input::Number(n) => n,
            Input::Quit => break,
            Input::Back => unreachable!(),
        };
        write!(stdout, "\n")?;

        let (passed_branches, failed_branches): (Vec<_>, Vec<_>) =
            cards[n].branches.iter().partition(|b| !b.failed);

        for (i, b) in passed_branches.iter().enumerate() {
            print_hr(&mut stdout)?;
            writeln!(stdout, "{}: {}", i.to_string().bold(), b.title.italic())?;
            writeln!(stdout, "   {}", b.description)?;
        }

        for (i, b) in failed_branches.iter().enumerate() {
            print_hr(&mut stdout)?;
            writeln!(
                stdout,
                "{}: {} (unavailable)",
                (i + passed_branches.len()).to_string().dimmed(),
                b.title.italic().dimmed()
            )?;
            writeln!(stdout, "   {}", b.description)?;
        }

        print_hr(&mut stdout)?;

        if passed_branches.len() > 0 {
            write!(
                stdout,
                "Select a branch {}, press {} to go back, or {} to quit: ",
                format!("(0-{})", passed_branches.len() - 1).bold(),
                "b".bold(),
                "q".bold()
            )?;
            stdout.flush()?;
        } else {
            write!(stdout, "\n")?;
            writeln!(
                stdout,
                "No choices are available and you will have to go back."
            )?;
            wait_for_enter(&mut stdout)?;
            continue;
        }

        let n = match get_input(passed_branches.len() - 1, true)? {
            Input::Number(n) => n,
            Input::Back => continue,
            Input::Quit => break,
        };
        write!(stdout, "\n")?;

        let branch_result = context.select_branch(&passed_branches[n].id);
        writeln!(stdout, "{}", branch_result.title.italic())?;
        writeln!(stdout, "{}", branch_result.description)?;

        for effect in &branch_result.effects {
            match effect {
                storylets::BranchResultEffect::QualityChanged {
                    quality,
                    description: Some(description),
                    ..
                } => {
                    let quality_properties = context.get_quality_properties(quality);
                    writeln!(
                        stdout,
                        "{}: {}",
                        quality_properties
                            .and_then(|p| p.title.as_ref())
                            .unwrap_or(&"Unnamed".to_string()),
                        description
                    )?;
                }
                _ => (),
            }
        }
        write!(stdout, "\n")?;
        wait_for_enter(&mut stdout)?;
    }

    Ok(())
}

enum Input {
    Number(usize),
    Back,
    Quit,
}

fn get_input(max: usize, allow_back: bool) -> io::Result<Input> {
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        if allow_back && input.trim() == "b" {
            return Ok(Input::Back);
        }

        if input.trim() == "q" {
            return Ok(Input::Quit);
        }

        let n = usize::from_str(&input.trim()).ok();
        if n.is_some() && n.unwrap() <= max {
            return Ok(Input::Number(n.unwrap()));
        }
    }
}

fn wait_for_enter(stdout: &mut std::io::Stdout) -> io::Result<()> {
    writeln!(stdout, "Press enter to continue...")?;
    stdout.flush()?;
    io::stdin().read_line(&mut String::new())?;
    Ok(())
}

fn print_hr(stdout: &mut std::io::Stdout) -> io::Result<()> {
    writeln!(stdout, "----------------------------------")
}
