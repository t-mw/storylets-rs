pub fn pluralize(s: &str) -> String {
    match s.to_lowercase().as_ref() {
        "energy" | "reputation" => return s.to_string(),
        "mouse" => return "mice".to_string(),
        _ => (),
    }

    let mut s = s.to_string();
    match s
        .chars()
        .rev()
        .next()
        .expect("Can not pluralize empty string")
    {
        's' | 'h' | 'x' => s + "es",
        'y' => match s.chars().rev().skip(1).next() {
            Some(c) if !is_vowel(c) => {
                s.pop();
                s + "ies"
            }
            _ => s + "s",
        },
        _ => s + "s",
    }
}

fn is_vowel(c: char) -> bool {
    match c.to_lowercase().next() {
        Some('a') | Some('e') | Some('i') | Some('o') | Some('u') => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::pluralize;

    #[test]
    fn test_pluralize() {
        assert_eq!(pluralize("journey"), "journeys".to_string());
        assert_eq!(pluralize("baby"), "babies".to_string());
        assert_eq!(pluralize("zebra"), "zebras".to_string());
        assert_eq!(pluralize("fox"), "foxes".to_string());
    }
}
