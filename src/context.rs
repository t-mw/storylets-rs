use crate::grammar;

use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::str::FromStr;

use rand::Rng;
use regex::{Regex, RegexBuilder};
use throne::{PhraseGroup, PhraseString};
use unidecode::unidecode;

pub struct Context {
    throne_context: throne::Context,
    quality_properties: HashMap<String, QualityProperty>,
}

pub struct Script {
    throne_context: throne::Context,
    quality_properties: HashMap<String, QualityProperty>,
}

#[derive(Clone, Debug, Default)]
pub struct QualityProperty {
    pub title: Option<String>,
    level_descriptions: Vec<QualityLevelDescription>,
    level_change_descriptions: Vec<QualityLevelChangeDescription>,
    hide_level: bool,
    hide_level_change: bool,
    is_thing: bool,
    is_global: bool,
}

impl QualityProperty {
    fn pluralized_title(&self) -> Option<String> {
        if self.is_thing {
            self.title.as_ref().map(|s| grammar::pluralize(&s))
        } else {
            self.title.clone()
        }
    }

    fn description_for_level(&self, level: i32) -> Option<String> {
        let mut level_description = None;

        for d in self.level_descriptions.iter() {
            if level >= d.level {
                level_description = Some(d.description.to_string());
            } else {
                break;
            }
        }

        if self.is_thing {
            let description = self.title.as_ref().map(|s| {
                if level > 1 {
                    grammar::pluralize(&s)
                } else {
                    s.to_string()
                }
            });

            if let Some(level_description) = level_description {
                if self.hide_level {
                    description.map(|description| format!("{} {}", level_description, description))
                } else {
                    description.map(|description| {
                        format!("{} {} ({})", level, level_description, description)
                    })
                }
            } else {
                if self.hide_level {
                    description
                } else {
                    description.map(|description| format!("{} {}", level, description))
                }
            }
        } else {
            let description = self.title.as_ref().map(|s| s.to_string());

            if let Some(level_description) = level_description {
                if self.hide_level {
                    description.map(|description| format!("{}: {}", description, level_description))
                } else {
                    description.map(|description| {
                        format!("{}: {} ({})", description, level_description, level)
                    })
                }
            } else {
                if self.hide_level {
                    description
                } else {
                    description.map(|description| format!("{}: {}", description, level))
                }
            }
        }
    }

    fn description_for_level_change(&self, level_before: i32, level_after: i32) -> Option<String> {
        let diff = level_after - level_before;

        let mut level_before_description_idx = None;
        let mut level_after_description_idx = None;

        for (i, d) in self.level_change_descriptions.iter().enumerate() {
            if level_before >= d.level {
                level_before_description_idx = Some(i);
            } else {
                break;
            }
        }

        for (i, d) in self.level_change_descriptions.iter().enumerate() {
            if level_after >= d.level {
                level_after_description_idx = Some(i);
            } else {
                break;
            }
        }

        let level_change_description =
            match (level_after_description_idx, level_before_description_idx) {
                (Some(a), None) => Some(&self.level_change_descriptions[a].description),
                (Some(a), Some(b)) if a != b => {
                    Some(&self.level_change_descriptions[a].description)
                }
                _ => None,
            };

        if self.is_thing {
            let plural_description = self.title.as_ref().map(|s| grammar::pluralize(&s));

            let diff_description = self.title.as_ref().map(|s| {
                if diff.abs() > 1 {
                    grammar::pluralize(&s)
                } else {
                    s.to_string()
                }
            });

            let description = self.title.as_ref().map(|s| {
                if level_after > 1 {
                    grammar::pluralize(&s)
                } else {
                    s.to_string()
                }
            });

            if let Some(level_change_description) = level_change_description {
                if self.hide_level_change {
                    Some(level_change_description.to_string())
                } else {
                    Some(format!(
                        "{} (new total: {})",
                        level_change_description, level_after
                    ))
                }
            } else {
                if self.hide_level_change {
                    plural_description.map(|plural_description| {
                        format!(
                            "You {} some {}",
                            if diff > 0 { "gained" } else { "lost" },
                            plural_description
                        )
                    })
                } else {
                    if level_before == 0 {
                        description.map(|description| {
                            format!("You now have {} {}", level_after, description)
                        })
                    } else {
                        diff_description.map(|diff_description| {
                            format!(
                                "You {} {} {} (new total: {})",
                                if diff > 0 { "gained" } else { "lost" },
                                diff.abs(),
                                diff_description,
                                level_after
                            )
                        })
                    }
                }
            }
        } else {
            let description = self.title.as_ref().map(|s| s.to_string());

            if let Some(level_change_description) = level_change_description {
                if self.hide_level_change {
                    Some(level_change_description.to_string())
                } else {
                    Some(format!(
                        "{} (new level: {})",
                        level_change_description, level_after
                    ))
                }
            } else {
                if self.hide_level_change {
                    if level_before == 0 {
                        description.map(|description| {
                            format!("You now have the '{}' quality", description)
                        })
                    } else if level_before == 0 {
                        description.map(|description| {
                            format!("You no longer have the '{}' quality", description)
                        })
                    } else {
                        description.map(|description| {
                            format!(
                                "Your '{}' quality {}",
                                description,
                                if diff > 0 { "increased" } else { "decreased" },
                            )
                        })
                    }
                } else {
                    if level_before == 0 {
                        description.map(|description| {
                            format!("Your '{}' quality is now {}", description, level_after)
                        })
                    } else {
                        description.map(|description| {
                            format!(
                                "Your '{}' quality {} by {} (new level: {})",
                                description,
                                if diff > 0 { "increased" } else { "decreased" },
                                diff.abs(),
                                level_after
                            )
                        })
                    }
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
struct QualityLevelDescription {
    level: i32,
    description: String,
}

#[derive(Clone, Debug)]
struct QualityLevelChangeDescription {
    level: i32,
    description: String,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Card {
    pub id: String,
    pub title: String,
    pub description: String,
    pub requirements: Vec<CardRequirement>,
    pub branches: Vec<Branch>,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Branch {
    pub id: String,
    pub title: String,
    pub description: String,
    pub failed: bool,
    pub requirements: Vec<BranchRequirement>,

    result_weights: Vec<ResultWeight>,
}

impl Branch {
    pub fn get_requirement_descriptions(&self, context: &Context) -> Vec<String> {
        if !self.failed {
            return vec![];
        }

        self.requirements
            .iter()
            .filter(|r| r.failed)
            .filter_map(|r| {
                let description = context
                    .quality_properties
                    .get(&r.quality)
                    .and_then(|properties| properties.title.as_ref().map(|s| s.to_string()));

                if let Some(description) = description {
                    Some(r.condition.failure_description(&description))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_difficulty_description(&self, context: &Context) -> Option<String> {
        // assume only one difficulty among results
        for w in &self.result_weights {
            if let Some(quality_id) = &w.difficulty_quality {
                let description = context
                    .quality_properties
                    .get(quality_id)
                    .and_then(|properties| properties.title.as_ref().map(|s| s.to_string()));

                if let Some(description) = description {
                    return Some(format!(
                        "Your '{}' quality gives you a {}% chance of success",
                        description, w.probability
                    ));
                }
            }
        }

        None
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ResultWeight {
    result: String,
    probability: i32,
    difficulty_quality: Option<String>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct BranchResult {
    pub id: String,
    pub title: String,
    pub description: String,
    pub effects: Vec<BranchResultEffect>,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum BranchResultEffect {
    QualityChanged {
        quality: String,
        diff: i32,
        value: i32,
        description: Option<String>,
    },
    AddPhrase {
        phrase: throne::VecPhrase,
        phrase_string: String,
    },
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CardRequirement {
    pub quality: String,
    pub failed: bool,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct BranchRequirement {
    pub quality: String,
    pub condition: RequirementCondition,
    pub failed: bool,
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RequirementCondition {
    Exists,
    Missing,
    Eq(i32),
    Neq(i32),
    Lt(i32),
    LtEq(i32),
    Gt(i32),
    GtEq(i32),
}

impl RequirementCondition {
    fn failure_description(&self, quality_description: &str) -> String {
        match self {
            RequirementCondition::Exists => {
                format!("You must have the '{}' quality", quality_description)
            }
            RequirementCondition::Missing => {
                format!("You already have the '{}' quality", quality_description)
            }
            RequirementCondition::Eq(level) => format!(
                "Your '{}' quality must be equal to {}",
                quality_description, level
            ),
            RequirementCondition::Neq(level) => format!(
                "Your '{}' quality should not be equal to {}",
                quality_description, level
            ),
            RequirementCondition::Lt(level) => format!(
                "Your '{}' quality must be less than {}",
                quality_description, level
            ),
            RequirementCondition::LtEq(level) => format!(
                "Your '{}' quality must be less than or equal to {}",
                quality_description, level
            ),
            RequirementCondition::Gt(level) => format!(
                "Your '{}' quality must be greater than {}",
                quality_description, level
            ),
            RequirementCondition::GtEq(level) => format!(
                "Your '{}' quality must be greater than or equal to {}",
                quality_description, level
            ),
        }
    }
}

impl RequirementCondition {
    fn from_str(s: &str, level: Option<i32>) -> Self {
        let expect_msg = format!("Missing level for {} condition", s);
        match s {
            "exists" => RequirementCondition::Exists,
            "missing" => RequirementCondition::Missing,
            "eq" => RequirementCondition::Eq(level.expect(&expect_msg)),
            "neq" => RequirementCondition::Neq(level.expect(&expect_msg)),
            "lt" => RequirementCondition::Lt(level.expect(&expect_msg)),
            "lt-eq" => RequirementCondition::LtEq(level.expect(&expect_msg)),
            "gt" => RequirementCondition::Gt(level.expect(&expect_msg)),
            "gt-eq" => RequirementCondition::GtEq(level.expect(&expect_msg)),
            _ => unreachable!("Unhandled condition: {}", s),
        }
    }
}

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct Quality {
    pub id: String,
    pub value: i32,
    pub title: Option<String>,
    pub description: Option<String>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            throne_context: throne::Context::from_text(""),
            quality_properties: HashMap::new(),
        }
    }

    pub fn from_text(text: &str) -> Self {
        let script = Script::from_text(text);
        Self::from_script(script)
    }

    #[cfg(test)]
    fn from_throne_text(text: &str) -> Self {
        let script = Script::from_throne_text(text);
        Self::from_script(script)
    }

    fn from_script(script: Script) -> Self {
        let mut context = Self {
            throne_context: throne::ContextBuilder::new().build(),
            quality_properties: HashMap::new(),
        };
        context.set_active_script(&script);
        context
    }

    pub fn set_active_script(&mut self, script: &Script) {
        let throne_context = &mut self.throne_context;
        self.quality_properties.retain(|id, props| {
            if props.is_global {
                true
            } else {
                queue_set_quality(throne_context, id, 0);
                false
            }
        });
        self.throne_context.update(|_: &throne::Phrase| None);

        let mut new_context = script.throne_context.clone();
        new_context.extend_state_from_context(&self.throne_context);

        self.throne_context = new_context;
        self.quality_properties
            .extend(script.quality_properties.clone());
    }

    pub fn draw_cards(&mut self) -> Vec<Card> {
        self.reset_state();

        let throne_context = &mut self.throne_context;

        let difficulty_probability_atom =
            throne_context.str_to_existing_atom("difficulty-probability");

        throne_context.append_state("#draw-cards");

        let mut core = &mut throne_context.core;
        let string_cache = &throne_context.string_cache;

        throne::update(&mut core, |p: &throne::Phrase| match p[0].atom {
            a if difficulty_probability_atom == Some(a) => {
                let n = throne::StringCache::atom_to_number(p[1].atom)
                    .expect("difficulty-probability must be passed a number");
                let n_difficulty = throne::StringCache::atom_to_number(p[2].atom)
                    .expect("difficulty-probability must be passed a number");

                let prob_float = ((n as f32 / n_difficulty as f32) * 0.6).max(0.01).min(1.0);
                let prob = (prob_float * 100.0).round() as i32;

                let mut p = p.to_vec();
                p[3] = throne::Token::new_number(prob, 0, 0);
                Some(p)
            }
            _ => unreachable!(p.to_string(&string_cache)),
        });

        self.get_cards()
    }

    pub fn select_branch(&mut self, branch: &str) -> BranchResult {
        let throne_context = &mut self.throne_context;

        let test_probability_atom = throne_context.str_to_existing_atom("test-probability");

        throne_context.append_state(&format!("#apply-branch {}", branch));

        let mut core = &mut throne_context.core;
        let string_cache = &throne_context.string_cache;

        // ensure that we only test each result once
        let mut tested_results_set = HashSet::new();

        throne::update(&mut core, |p: &throne::Phrase| match p[0].atom {
            a if test_probability_atom == Some(a) => {
                let result_atom = p
                    .get(1)
                    .map(|t| t.atom)
                    .expect("test-probability missing first argument");

                if tested_results_set.contains(&result_atom) {
                    return None;
                }

                tested_results_set.insert(result_atom);

                let prob = p
                    .get(2)
                    .and_then(|t| throne::StringCache::atom_to_number(t.atom))
                    .expect("test-probability must be passed a number");

                let mut rng = rand::thread_rng();
                if rng.gen_ratio(
                    prob.try_into()
                        .expect("Probability should be a positive number"),
                    100,
                ) {
                    Some(p.to_vec())
                } else {
                    None
                }
            }
            _ => unreachable!(p.to_string(&string_cache)),
        });

        let throne_context = &self.throne_context;
        let branch_result = if let (
            Some(branch_atom),
            Some(result_atom),
            Some(title_atom),
            Some(description_atom),
            Some(branch_id_atom),
        ) = (
            throne_context.str_to_existing_atom("branch"),
            throne_context.str_to_existing_atom("result"),
            throne_context.str_to_existing_atom("title"),
            throne_context.str_to_existing_atom("description"),
            throne_context.str_to_existing_atom(branch),
        ) {
            throne_context
                .find_phrases_exactly4(
                    Some(&branch_atom),
                    Some(&branch_id_atom),
                    Some(&result_atom),
                    None,
                )
                .get(0)
                .map(|p| {
                    let result_id_atom = p[3].atom;
                    let result_id = throne_context.atom_to_str(result_id_atom).to_string();

                    let title = throne_context
                        .find_phrase3(Some(&result_atom), Some(&result_id_atom), Some(&title_atom))
                        .and_then(|p| p.get(3))
                        .map(|t| throne_context.atom_to_str(t.atom).to_string())
                        .expect(&format!("Missing title for result '{}'", result_id));

                    let description = throne_context
                        .find_phrase3(
                            Some(&result_atom),
                            Some(&result_id_atom),
                            Some(&description_atom),
                        )
                        .and_then(|p| p.get(3))
                        .map(|t| throne_context.atom_to_str(t.atom).to_string())
                        .expect(&format!("Missing description for result '{}'", result_id));

                    let effects = self.get_branch_result_effects(&result_id);

                    BranchResult {
                        id: result_id,
                        title,
                        description: format_description(&description),
                        effects,
                    }
                })
        } else {
            None
        }
        .expect(&format!(
            "Missing result for branch '{}'. Did you draw cards before selecting the branch?",
            branch
        ));

        self.reset_state();

        branch_result
    }

    fn get_branch_result_effects(&self, result_id: &str) -> Vec<BranchResultEffect> {
        let throne_context = &self.throne_context;

        let mut effects = if let (Some(result_atom), Some(effect_atom), Some(result_id_atom)) = (
            throne_context.str_to_existing_atom("result"),
            throne_context.str_to_existing_atom("effect"),
            throne_context.str_to_existing_atom(result_id),
        ) {
            throne_context
                .find_phrases3(
                    Some(&result_atom),
                    Some(&result_id_atom),
                    Some(&effect_atom),
                )
                .iter()
                .map(|p| {
                    let effect_type = p.get(3).map(|t| throne_context.atom_to_str(t.atom));

                    match effect_type {
                        Some("quality-changed") => {
                            let quality_id_atom = p
                                .get(4)
                                .map(|t| t.atom)
                                .expect("quality-changed effect missing quality");

                            let quality = throne_context.atom_to_str(quality_id_atom).to_string();

                            let n_before = p
                                .get(5)
                                .map(|t| {
                                    throne_context.atom_to_number(t.atom).unwrap_or_else(|| {
                                        panic!(
                                            "quality-changed n_before '{}' is not a number",
                                            throne_context.atom_to_str(t.atom)
                                        )
                                    })
                                })
                                .expect("quality-changed effect missing n_before");

                            let n = p
                                .get(6)
                                .map(|t| {
                                    throne_context.atom_to_number(t.atom).unwrap_or_else(|| {
                                        panic!(
                                            "quality-changed n '{}' is not a number",
                                            throne_context.atom_to_str(t.atom)
                                        )
                                    })
                                })
                                .expect("quality-changed effect missing n");

                            let description =
                                self.quality_properties
                                    .get(&quality)
                                    .and_then(|properties| {
                                        properties.description_for_level_change(n_before, n)
                                    });

                            BranchResultEffect::QualityChanged {
                                quality,
                                diff: n - n_before,
                                value: n,
                                description: description.map(|s| format_description(&s)),
                            }
                        }
                        Some("add-phrase") => {
                            let phrase = p
                                .get_group(4)
                                .expect("Missing phrase for add-phrase result")
                                .normalize();
                            let phrase_string =
                                throne::build_phrase(&phrase, &throne_context.string_cache);

                            BranchResultEffect::AddPhrase {
                                phrase,
                                phrase_string,
                            }
                        }
                        _ => {
                            unreachable!(format!("unhandled result effect type: {:?}", effect_type))
                        }
                    }
                })
                .collect()
        } else {
            vec![]
        };

        effects.sort();

        effects
    }

    pub fn set_quality(&mut self, id: &str, value: i32) {
        queue_set_quality(&mut self.throne_context, id, value);
        self.throne_context.update(|_: &throne::Phrase| None);

        self.reset_state();
    }

    pub fn set_quality_bool(&mut self, id: &str, value: bool) {
        self.set_quality(id, if value { 1 } else { 0 });
    }

    pub fn get_throne_context(&self) -> &throne::Context {
        &self.throne_context
    }

    fn get_cards(&self) -> Vec<Card> {
        let throne_context = &self.throne_context;
        let mut cards = if let (Some(card_atom), Some(title_atom), Some(description_atom)) = (
            throne_context.str_to_existing_atom("card"),
            throne_context.str_to_existing_atom("title"),
            throne_context.str_to_existing_atom("description"),
        ) {
            throne_context
                .find_phrases_exactly2(Some(&card_atom), None)
                .iter()
                .map(|p| {
                    let card_id_atom = p[1].atom;
                    let card_id = throne_context.atom_to_str(card_id_atom).to_string();
                    let requirements = self.get_card_requirements(&card_id);
                    let branches = self.get_branches(&card_id);

                    let title = throne_context
                        .find_phrase3(Some(&card_atom), Some(&card_id_atom), Some(&title_atom))
                        .and_then(|p| p.get(3))
                        .map(|t| throne_context.atom_to_str(t.atom).to_string())
                        .expect(&format!("Missing title for card '{}'", card_id));

                    let description = throne_context
                        .find_phrase3(
                            Some(&card_atom),
                            Some(&card_id_atom),
                            Some(&description_atom),
                        )
                        .and_then(|p| p.get(3))
                        .map(|t| throne_context.atom_to_str(t.atom).to_string())
                        .unwrap_or("".to_string());

                    Card {
                        id: card_id,
                        title,
                        description: format_description(&description),
                        requirements,
                        branches,
                    }
                })
                .collect()
        } else {
            vec![]
        };

        cards.sort();

        cards
    }

    fn get_card_requirements(&self, card_id: &str) -> Vec<CardRequirement> {
        let throne_context = &self.throne_context;

        if let (Some(card_atom), Some(card_id_atom)) = (
            throne_context.str_to_existing_atom("card"),
            throne_context.str_to_existing_atom(card_id),
        ) {
            let mut requirements = if let Some(passed_quality_atom) =
                throne_context.str_to_existing_atom("passed-quality")
            {
                throne_context
                    .find_phrases3(
                        Some(&card_atom),
                        Some(&card_id_atom),
                        Some(&passed_quality_atom),
                    )
                    .iter()
                    .map(|p| {
                        let quality = throne_context.atom_to_str(p[4].atom).to_string();
                        CardRequirement {
                            quality,
                            failed: false,
                        }
                    })
                    .collect()
            } else {
                vec![]
            };

            requirements.sort();

            requirements
        } else {
            vec![]
        }
    }

    fn get_branches(&self, card_id: &str) -> Vec<Branch> {
        let throne_context = &self.throne_context;

        let mut branches = if let (
            Some(card_atom),
            Some(card_id_atom),
            Some(branch_atom),
            Some(title_atom),
            Some(description_atom),
            Some(result_probability_atom),
        ) = (
            throne_context.str_to_existing_atom("card"),
            throne_context.str_to_existing_atom(card_id),
            throne_context.str_to_existing_atom("branch"),
            throne_context.str_to_existing_atom("title"),
            throne_context.str_to_existing_atom("description"),
            throne_context.str_to_existing_atom("result-probability"),
        ) {
            throne_context
                .find_phrases_exactly4(
                    Some(&card_atom),
                    Some(&card_id_atom),
                    Some(&branch_atom),
                    None,
                )
                .iter()
                .map(|p| {
                    let branch_id_atom = p[3].atom;
                    let branch_id = throne_context.atom_to_str(branch_id_atom).to_string();
                    let requirements = self.get_branch_requirements(&branch_id);
                    let failed = requirements.iter().any(|r| r.failed);

                    let title = throne_context
                        .find_phrase3(Some(&branch_atom), Some(&branch_id_atom), Some(&title_atom))
                        .and_then(|p| p.get(3))
                        .map(|t| throne_context.atom_to_str(t.atom).to_string())
                        .expect(&format!("Missing title for branch '{}'", branch_id));

                    let description = throne_context
                        .find_phrase3(
                            Some(&branch_atom),
                            Some(&branch_id_atom),
                            Some(&description_atom),
                        )
                        .and_then(|p| p.get(3))
                        .map(|t| throne_context.atom_to_str(t.atom).to_string())
                        .unwrap_or("".to_string());

                    let result_weights = throne_context
                        .find_phrases3(
                            Some(&branch_atom),
                            Some(&branch_id_atom),
                            Some(&result_probability_atom),
                        )
                        .iter()
                        .map(|p| {
                            let result_id_atom = p[3].atom;
                            let probability_atom = p[4].atom;
                            let quality_id_atom = p.get(5).map(|t| t.atom);

                            let result_id = throne_context.atom_to_str(result_id_atom);
                            let probability = throne_context
                                .atom_to_number(probability_atom)
                                .expect("Result probability is not a number");
                            let quality_id =
                                quality_id_atom.map(|a| throne_context.atom_to_str(a).to_string());

                            ResultWeight {
                                result: result_id.to_string(),
                                probability,
                                difficulty_quality: quality_id,
                            }
                        })
                        .collect();

                    Branch {
                        id: branch_id,
                        title,
                        description: format_description(&description),
                        failed,
                        requirements,
                        result_weights,
                    }
                })
                .collect()
        } else {
            vec![]
        };

        branches.sort();

        branches
    }

    fn get_branch_requirements(&self, branch_id: &str) -> Vec<BranchRequirement> {
        let throne_context = &self.throne_context;

        if let (Some(branch_atom), Some(branch_id_atom)) = (
            throne_context.str_to_existing_atom("branch"),
            throne_context.str_to_existing_atom(branch_id),
        ) {
            let mut passed_requirements = if let Some(passed_quality_atom) =
                throne_context.str_to_existing_atom("passed-quality")
            {
                throne_context
                    .find_phrases3(
                        Some(&branch_atom),
                        Some(&branch_id_atom),
                        Some(&passed_quality_atom),
                    )
                    .iter()
                    .map(|p| {
                        let quality = throne_context.atom_to_str(p[4].atom).to_string();

                        let level = p.get(5).and_then(|t| throne_context.atom_to_number(t.atom));
                        let condition = RequirementCondition::from_str(
                            throne_context.atom_to_str(p[3].atom),
                            level,
                        );
                        BranchRequirement {
                            quality,
                            condition,
                            failed: false,
                        }
                    })
                    .collect()
            } else {
                vec![]
            };

            let mut failed_requirements = if let Some(failed_quality_atom) =
                throne_context.str_to_existing_atom("failed-quality")
            {
                throne_context
                    .find_phrases3(
                        Some(&branch_atom),
                        Some(&branch_id_atom),
                        Some(&failed_quality_atom),
                    )
                    .iter()
                    .map(|p| {
                        let quality = throne_context.atom_to_str(p[4].atom).to_string();

                        let level = p.get(5).and_then(|t| throne_context.atom_to_number(t.atom));
                        let condition = RequirementCondition::from_str(
                            throne_context.atom_to_str(p[3].atom),
                            level,
                        );
                        BranchRequirement {
                            quality,
                            condition,
                            failed: true,
                        }
                    })
                    .collect()
            } else {
                vec![]
            };

            let mut requirements = vec![];
            requirements.append(&mut passed_requirements);
            requirements.append(&mut failed_requirements);

            requirements.sort();

            requirements
        } else {
            vec![]
        }
    }

    pub fn get_qualities(&self) -> Vec<Quality> {
        let throne_context = &self.throne_context;

        let mut qualities =
            if let Some(quality_atom) = throne_context.str_to_existing_atom("quality") {
                throne_context
                    .find_phrases_exactly3(Some(&quality_atom), None, None)
                    .iter()
                    .map(|p| {
                        let id_atom = p[1].atom;
                        let id = throne_context.atom_to_str(id_atom).to_string();
                        let value = throne_context.atom_to_number(p[2].atom).unwrap();

                        let properties = self.quality_properties.get(&id);
                        let title = properties.and_then(|properties| properties.pluralized_title());
                        let description = properties
                            .and_then(|properties| properties.description_for_level(value));

                        Quality {
                            id,
                            value,
                            title,
                            description: description.map(|s| format_description(&s)),
                        }
                    })
                    .collect()
            } else {
                vec![]
            };

        qualities.sort();

        qualities
    }

    pub fn get_quality_properties(&self, id: &str) -> Option<&QualityProperty> {
        self.quality_properties.get(id)
    }

    fn reset_state(&mut self) {
        let throne_context = &mut self.throne_context;

        // retain quality state
        let quality_atom = throne_context.str_to_atom("quality");

        let mut new_state = throne::State::new();
        for phrase_id in throne_context.core.state.iter() {
            let p = throne_context.core.state.get(*phrase_id);
            if p[0].atom == quality_atom {
                new_state.push(p.to_vec());
            }
        }
        throne_context.core.state = new_state;
    }
}

fn queue_set_quality(throne_context: &mut throne::Context, id: &str, value: i32) {
    throne_context.append_state(&format!("#set-quality {} {}", id, value));
}

impl Script {
    pub fn from_text(text: &str) -> Self {
        let text = expand_concise_syntax(text);
        Self::from_throne_text(&text)
    }

    fn from_throne_text(text: &str) -> Self {
        let base_txt = include_str!("storylets.throne");
        let quality_properties = read_quality_properties(&text);

        let throne_context = throne::ContextBuilder::new()
            .text(&(text.to_string() + "\n" + base_txt))
            .build();

        Self {
            throne_context,
            quality_properties,
        }
    }
}

fn read_quality_properties(txt: &str) -> HashMap<String, QualityProperty> {
    let quality_regex = RegexBuilder::new(r"^\s*<<(quality[\w-]+) ([\w-]*).*$")
        .multi_line(true)
        .build()
        .unwrap();

    let mut all_quality_properties: HashMap<String, QualityProperty> = HashMap::new();
    for caps in quality_regex.captures_iter(txt) {
        let quality_id = caps.get(2).unwrap().as_str();
        let quality_properties = all_quality_properties
            .entry(quality_id.to_string())
            .or_insert(Default::default());

        let property_type = caps.get(1).unwrap().as_str();
        let split = caps.get(0).unwrap().as_str().split(" ").collect::<Vec<_>>();

        let get_final_string = |start_idx| {
            split
                .get(start_idx..)
                .filter(|s| s.len() > 0)
                .map(|s| s.join(" ").trim_matches('`').to_string())
        };

        match property_type {
            "quality-title" => {
                quality_properties.title = Some(get_final_string(2).expect(&format!(
                    "Missing title for quality-title for quality '{}'",
                    quality_id
                )));
            }
            "quality-level-description" => {
                quality_properties
                    .level_descriptions
                    .push(QualityLevelDescription {
                        level: split
                            .get(2)
                            .and_then(|s| i32::from_str(s).ok())
                            .expect(&format!(
                                "Missing level for quality-level-description for \
                 quality '{}'",
                                quality_id
                            )),
                        description: get_final_string(3).expect(&format!(
                            "Missing description for quality-level-description for \
               quality '{}'",
                            quality_id
                        )),
                    });
            }
            "quality-level-change-description" => {
                quality_properties
                    .level_change_descriptions
                    .push(QualityLevelChangeDescription {
                        level: split
                            .get(2)
                            .and_then(|s| i32::from_str(s).ok())
                            .expect(&format!(
                                "Missing level for quality-level-change-description for \
                 quality '{}'",
                                quality_id
                            )),
                        description: get_final_string(3).expect(&format!(
                            "Missing description for quality-level-change-description \
               for quality '{}'",
                            quality_id
                        )),
                    });
            }
            "quality-hide-level" => quality_properties.hide_level = true,
            "quality-hide-level-change" => quality_properties.hide_level_change = true,
            "quality-is-thing" => quality_properties.is_thing = true,
            "quality-is-global" => quality_properties.is_global = true,
            _ => unreachable!("Unhandled quality property type: {}", property_type),
        }
    }

    let mut quality_properties = HashMap::new();
    for (quality_id, mut properties) in all_quality_properties {
        properties.level_descriptions.sort_by_key(|d| d.level);
        properties
            .level_change_descriptions
            .sort_by_key(|d| d.level);

        quality_properties.insert(quality_id, properties);
    }
    quality_properties
}

fn expand_concise_syntax(text: &str) -> String {
    let mut out_lines = vec![];

    enum CurrentType {
        Card,
        Branch,
        Result,
        Quality,
    }

    impl CurrentType {
        fn prefix(&self) -> &'static str {
            match self {
                CurrentType::Card => "card",
                CurrentType::Branch => "branch",
                CurrentType::Result => "result",
                CurrentType::Quality => "quality",
            }
        }
    }

    let mut current_type: Option<CurrentType> = None;
    let mut current_type_id: Option<String> = None;
    let mut current_card_id: Option<String> = None;
    let mut current_branch_id: Option<String> = None;

    let mut id_counter = 0;

    let banned_id_chars_re = Regex::new(r"[^a-zA-Z0-9'_-]").unwrap();

    let generate_id = |current_type: &Option<CurrentType>, id_counter: &mut i32| {
        let prefix = current_type
            .as_ref()
            .map(|t| t.prefix())
            .expect("Missing a type declaration");

        *id_counter += 1;
        format!("{}-{}", prefix, *id_counter)
    };

    let write_relation = |current_type: &Option<CurrentType>,
                          current_type_id: &Option<String>,
                          current_card_id: &Option<String>,
                          current_branch_id: &Option<String>,
                          out_lines: &mut Vec<String>| {
        let current_type_id = current_type_id.as_ref().expect("Missing type id");

        match current_type {
            Some(CurrentType::Branch) => {
                out_lines.push(format!(
                    "<<card-branch {} {}",
                    current_card_id.as_ref().expect(&format!(
                        "Branch '{}' must be preceded by a card declaration",
                        current_type_id
                    )),
                    current_type_id
                ));
            }
            Some(CurrentType::Result) => {
                out_lines.push(format!(
                    "<<branch-result {} {}",
                    current_branch_id.as_ref().expect(&format!(
                        "Result '{}' must be preceded by a branch declaration",
                        current_type_id
                    )),
                    current_type_id
                ));
            }
            _ => (),
        }
    };

    for (line_i, l) in text.lines().map(|l| l.trim()).enumerate() {
        let mut split = l.split_whitespace();

        match split.next() {
            Some("card") => {
                current_type = Some(CurrentType::Card);
                current_type_id = split.next().map(|s| s.to_string());
                current_card_id = current_type_id.clone();
            }
            Some("branch") => {
                current_type = Some(CurrentType::Branch);
                current_type_id = split.next().map(|s| s.to_string());
                if current_type_id.is_some() {
                    write_relation(
                        &current_type,
                        &current_type_id,
                        &current_card_id,
                        &current_branch_id,
                        &mut out_lines,
                    );
                }
                current_branch_id = current_type_id.clone();
            }
            Some("result") => {
                current_type = Some(CurrentType::Result);
                current_type_id = split.next().map(|s| s.to_string());
                if current_type_id.is_some() {
                    write_relation(
                        &current_type,
                        &current_type_id,
                        &current_card_id,
                        &current_branch_id,
                        &mut out_lines,
                    );
                }
            }
            Some("quality") => {
                current_type = Some(CurrentType::Quality);
                current_type_id = split.next().map(|s| s.to_string());
            }
            Some("title") => {
                let prefix = current_type.as_ref().map(|t| t.prefix()).expect(&format!(
                    "Line {}: Missing preceding type declaration",
                    line_i,
                ));

                let title = l
                    .get("title".len()..)
                    .map(|s| s.trim())
                    .filter(|s| s.len() > 0)
                    .expect(&format!("Line {}: Missing text for title", line_i));

                if current_type_id.is_none() {
                    let decoded = unidecode(title).to_lowercase();
                    let generated_id = banned_id_chars_re.replace_all(&decoded, "-");

                    id_counter += 1;
                    current_type_id = Some(format!("{}-{}", generated_id, id_counter));

                    match current_type {
                        Some(CurrentType::Card) => {
                            current_card_id = current_type_id.clone();
                        }
                        Some(CurrentType::Branch) => {
                            current_branch_id = current_type_id.clone();
                        }
                        _ => (),
                    }

                    write_relation(
                        &current_type,
                        &current_type_id,
                        &current_card_id,
                        &current_branch_id,
                        &mut out_lines,
                    );
                }

                out_lines.push(format!(
                    "<<{}-title {} `{}`",
                    prefix,
                    current_type_id.as_ref().unwrap(),
                    title
                ));
            }
            Some("description") => {
                let prefix = current_type.as_ref().map(|t| t.prefix()).expect(&format!(
                    "Line {}: Missing preceding type declaration",
                    line_i,
                ));

                let description = l
                    .get("description".len()..)
                    .map(|s| s.trim())
                    .filter(|s| s.len() > 0)
                    .expect(&format!("Line {}: Missing text for description", line_i));

                if current_type_id.is_none() {
                    current_type_id = Some(generate_id(&current_type, &mut id_counter));

                    match current_type {
                        Some(CurrentType::Card) => {
                            current_card_id = current_type_id.clone();
                        }
                        Some(CurrentType::Branch) => {
                            current_branch_id = current_type_id.clone();
                        }
                        _ => (),
                    }

                    write_relation(
                        &current_type,
                        &current_type_id,
                        &current_card_id,
                        &current_branch_id,
                        &mut out_lines,
                    );
                }

                out_lines.push(format!(
                    "<<{}-description {} `{}`",
                    prefix,
                    current_type_id.as_ref().unwrap(),
                    description
                ));
            }
            Some(command @ "level-description") | Some(command @ "level-change-description") => {
                let prefix = current_type.as_ref().map(|t| t.prefix()).expect(&format!(
                    "Line {}: Missing preceding type declaration",
                    line_i,
                ));

                let n = split
                    .next()
                    .expect(&format!("Line {}: Missing level for {}", line_i, command));

                let description = split.collect::<Vec<_>>().join(" ");
                if description.is_empty() {
                    panic!("Line {}: Missing description for {}", line_i, command);
                }

                if current_type_id.is_none() {
                    current_type_id = Some(generate_id(&current_type, &mut id_counter));
                }

                out_lines.push(format!(
                    "<<{}-{} {} {} `{}`",
                    prefix,
                    command,
                    current_type_id.as_ref().unwrap(),
                    n,
                    description
                ));
            }
            Some(command) => {
                let prefix = current_type.as_ref().map(|t| t.prefix()).expect(&format!(
                    "Line {}: Missing preceding type declaration",
                    line_i,
                ));

                if current_type_id.is_none() {
                    current_type_id = Some(generate_id(&current_type, &mut id_counter));

                    match current_type {
                        Some(CurrentType::Card) => {
                            current_card_id = current_type_id.clone();
                        }
                        Some(CurrentType::Branch) => {
                            current_branch_id = current_type_id.clone();
                        }
                        _ => (),
                    }

                    write_relation(
                        &current_type,
                        &current_type_id,
                        &current_card_id,
                        &current_branch_id,
                        &mut out_lines,
                    );
                }

                out_lines.push(
                    format!(
                        "<<{}-{} {} {}",
                        prefix,
                        command,
                        current_type_id.as_ref().unwrap(),
                        split.collect::<Vec<_>>().join(" ")
                    )
                    .trim()
                    .to_string(),
                );
            }
            None => (),
        }
    }

    out_lines.join("\n")
}

fn format_description(s: &str) -> String {
    s.replace("\\n", "\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_quality() {
        let mut context = Context::from_throne_text("");
        context.throne_context.print();
        context.set_quality("test", 0);
        assert_eq!(context.get_qualities(), vec![]);
    }

    #[test]
    fn test_populate_cards() {
        let mut context = Context::from_throne_text(include_str!("basic-test.throne"));

        context.draw_cards();

        context.throne_context.print();

        let cards = context.get_cards();
        assert_eq!(
            cards,
            vec![
                Card {
                    id: "treasure".to_string(),
                    title: "Treasure for all".to_string(),
                    description: "You see a gleaming metal chest".to_string(),
                    requirements: vec![CardRequirement {
                        quality: "treasure-open".to_string(),
                        failed: false
                    }],
                    branches: vec![
                        Branch {
                            id: "kick-chest".to_string(),
                            title: "Kick the chest".to_string(),
                            description: "It looks very sturdy, but then again you're very strong."
                                .to_string(),
                            failed: true,
                            requirements: vec![BranchRequirement {
                                quality: "strength".to_string(),
                                condition: RequirementCondition::Gt(2),
                                failed: true
                            }],
                            result_weights: vec![],
                        },
                        Branch {
                            id: "open-chest".to_string(),
                            title: "Open the chest".to_string(),
                            description: "The obvious choice - you'd be a fool not to \
                            take the opportunity."
                                .to_string(),
                            failed: false,
                            requirements: vec![],
                            result_weights: vec![],
                        }
                    ],
                },
                Card {
                    id: "waterfall".to_string(),
                    title: "Angel Falls".to_string(),
                    description: "The waterfall is crashing down and your throat is parched."
                        .to_string(),
                    requirements: vec![],
                    branches: vec![Branch {
                        id: "drink".to_string(),
                        title: "Satisfy your thirst".to_string(),
                        description:
                            "The water looks crystal clear, but is it really safe to drink."
                                .to_string(),
                        failed: false,
                        requirements: vec![],
                        result_weights: vec![],
                    }]
                }
            ]
        );
    }

    #[test]
    fn test_card_quality_missing_failed() {
        let mut context = Context::from_throne_text(include_str!("basic-test.throne"));

        context.set_quality("treasure-open", 1);
        context.draw_cards();

        context.throne_context.print();

        let cards = context.get_cards();
        assert_eq!(
            cards,
            vec![Card {
                id: "waterfall".to_string(),
                title: "Angel Falls".to_string(),
                description: "The waterfall is crashing down and your throat is parched."
                    .to_string(),
                requirements: vec![],
                branches: vec![Branch {
                    id: "drink".to_string(),
                    title: "Satisfy your thirst".to_string(),
                    description: "The water looks crystal clear, but is it really safe to drink."
                        .to_string(),
                    failed: false,
                    requirements: vec![],
                    result_weights: vec![],
                }]
            }]
        );

        let a1 = context.throne_context.str_to_atom("card");
        let a2 = context.throne_context.str_to_atom("treasure");
        let a3 = context.throne_context.str_to_atom("failed-quality");
        let a4 = context.throne_context.str_to_atom("missing");
        let a5 = context.throne_context.str_to_atom("treasure-open");
        assert_eq!(
            context
                .throne_context
                .find_phrases_exactly5(Some(&a1), Some(&a2), Some(&a3), Some(&a4), Some(&a5))
                .len(),
            1
        );
    }

    #[test]
    fn test_branch_quality_gt_failed() {
        let mut context = Context::from_throne_text(include_str!("basic-test.throne"));

        context.set_quality("strength", 2);
        context.draw_cards();

        context.throne_context.print();

        let branches = context.get_branches("treasure");
        assert_eq!(
            branches.iter().find(|b| b.id == "kick-chest"),
            Some(&Branch {
                id: "kick-chest".to_string(),
                title: "Kick the chest".to_string(),
                description: "It looks very sturdy, but then again you're very strong.".to_string(),
                failed: true,
                requirements: vec![BranchRequirement {
                    quality: "strength".to_string(),
                    condition: RequirementCondition::Gt(2),
                    failed: true
                }],
                result_weights: vec![],
            })
        );
    }

    #[test]
    fn test_branch_quality_gt_failed_message() {
        let mut context =
            Context::from_throne_text(include_str!("requirement-messages-test.throne"));

        context.set_quality("strength", 2);
        context.draw_cards();

        context.throne_context.print();

        let branches = context.get_branches("treasure");
        let branch = branches
            .iter()
            .find(|b| b.id == "kick-chest")
            .expect("Branch not found");
        assert_eq!(
            branch.get_requirement_descriptions(&context),
            vec!["Your 'Strength' quality must be greater than 2"]
        );
    }

    #[test]
    fn test_branch_quality_gt_passed() {
        let mut context = Context::from_throne_text(include_str!("basic-test.throne"));

        context.set_quality("strength", 3);
        context.draw_cards();

        context.throne_context.print();

        let branches = context.get_branches("treasure");
        assert_eq!(
            branches.iter().find(|b| b.id == "kick-chest"),
            Some(&Branch {
                id: "kick-chest".to_string(),
                title: "Kick the chest".to_string(),
                description: "It looks very sturdy, but then again you're very strong.".to_string(),
                failed: false,
                requirements: vec![BranchRequirement {
                    quality: "strength".to_string(),
                    condition: RequirementCondition::Gt(2),
                    failed: false
                }],
                result_weights: vec![],
            })
        );
    }

    #[test]
    fn test_select_branch_set_quality() {
        let mut context = Context::from_throne_text(include_str!("basic-test.throne"));

        context.draw_cards();

        context.throne_context.print();

        assert_eq!(context.get_qualities(), vec![]);

        let branch_result = context.select_branch("open-chest");
        assert_eq!(branch_result.id, "success");
        assert_eq!(
            branch_result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "treasure-open".to_string(),
                diff: 1,
                value: 1,
                description: None
            }]
        );

        context.throne_context.print();

        assert_eq!(
            context.get_qualities(),
            vec![Quality {
                id: "treasure-open".to_string(),
                value: 1,
                title: None,
                description: None,
            }]
        );
    }

    #[test]
    fn test_select_branch_change_quality() {
        let mut context = Context::from_throne_text(include_str!("quality-change-test.throne"));

        context.draw_cards();

        context.throne_context.print();

        assert_eq!(
            context.get_qualities(),
            vec![Quality {
                id: "jewel-bags".to_string(),
                value: 1,
                title: None,
                description: None
            }]
        );

        let branch_result = context.select_branch("open-chest");

        context.throne_context.print();

        assert_eq!(branch_result.id, "open-success");
        assert_eq!(
            branch_result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewel-bags".to_string(),
                diff: 1,
                value: 2,
                description: None
            }]
        );

        context.draw_cards();

        let branch_result = context.select_branch("kick-chest");

        context.throne_context.print();

        assert_eq!(branch_result.id, "kick-fail");
        assert_eq!(
            branch_result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewel-bags".to_string(),
                diff: -2,
                value: 0,
                description: None
            }]
        );

        assert_eq!(context.get_qualities(), vec![]);
    }

    #[test]
    fn test_select_branch_add_phrase() {
        let mut context = Context::from_throne_text(include_str!("add-phrase-test.throne"));

        context.draw_cards();

        let branch_result = context.select_branch("open-chest");

        context.throne_context.print();

        assert_eq!(branch_result.id, "success");
        assert_eq!(
            branch_result.effects,
            vec![BranchResultEffect::AddPhrase {
                phrase: throne::tokenize(
                    "open-treasure (1 2 3)",
                    &mut context.throne_context.string_cache
                ),
                phrase_string: "(open-treasure (1 2 3))".to_string()
            }]
        );
    }

    #[test]
    fn test_quality_descriptions() {
        let mut context =
            Context::from_throne_text(include_str!("quality-description-test.throne"));

        context.set_quality("season", 1);
        context.set_quality("strength", 1);
        context.set_quality("jewels", 1);

        context.throne_context.print();

        assert_eq!(
            context.get_qualities(),
            vec![
                Quality {
                    id: "jewels".to_string(),
                    value: 1,
                    title: Some("Jewels".to_string()),
                    description: Some("1 Jewel".to_string()),
                },
                Quality {
                    id: "season".to_string(),
                    value: 1,
                    title: Some("Current season".to_string()),
                    description: Some("Current season: Spring".to_string()),
                },
                Quality {
                    id: "strength".to_string(),
                    value: 1,
                    title: Some("Strength".to_string()),
                    description: Some("Strength: 1".to_string()),
                },
            ]
        );

        context.set_quality("season", 2);
        context.set_quality("strength", 2);
        context.set_quality("jewels", 2);

        context.throne_context.print();

        assert_eq!(
            context.get_qualities(),
            vec![
                Quality {
                    id: "jewels".to_string(),
                    value: 2,
                    title: Some("Jewels".to_string()),
                    description: Some("2 Jewels".to_string()),
                },
                Quality {
                    id: "season".to_string(),
                    value: 2,
                    title: Some("Current season".to_string()),
                    description: Some("Current season: Summer".to_string()),
                },
                Quality {
                    id: "strength".to_string(),
                    value: 2,
                    title: Some("Strength".to_string()),
                    description: Some("Strength: 2".to_string()),
                },
            ]
        );
    }

    #[test]
    fn test_quality_change_descriptions_increase() {
        let mut context =
            Context::from_throne_text(include_str!("quality-description-test.throne"));

        context.draw_cards();

        let result = context.select_branch("increase-season");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "season".to_string(),
                diff: 1,
                value: 1,
                description: Some("Spring is upon us".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-strength");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "strength".to_string(),
                diff: 1,
                value: 1,
                description: Some("Your 'Strength' quality is now 1".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-jewels");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewels".to_string(),
                diff: 1,
                value: 1,
                description: Some("You now have 1 Jewel".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-season");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "season".to_string(),
                diff: 1,
                value: 2,
                description: Some("Summer is upon us".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-strength");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "strength".to_string(),
                diff: 1,
                value: 2,
                description: Some(
                    "Your 'Strength' quality increased by 1 (new level: 2)".to_string()
                )
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-jewels");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewels".to_string(),
                diff: 1,
                value: 2,
                description: Some("You gained 1 Jewel (new total: 2)".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-season-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "season".to_string(),
                diff: 2,
                value: 4,
                description: Some("Winter is upon us".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-strength-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "strength".to_string(),
                diff: 2,
                value: 4,
                description: Some("You're slightly stronger (new level: 4)".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("increase-jewels-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewels".to_string(),
                diff: 2,
                value: 4,
                description: Some("You gained 2 Jewels (new total: 4)".to_string())
            }]
        );
    }

    #[test]
    fn test_quality_change_descriptions_decrease() {
        let mut context =
            Context::from_throne_text(include_str!("quality-description-test.throne"));

        context.set_quality("season", 4);
        context.set_quality("strength", 4);
        context.set_quality("jewels", 4);

        context.draw_cards();

        let result = context.select_branch("decrease-season-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "season".to_string(),
                diff: -2,
                value: 2,
                description: Some("Summer is upon us".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("decrease-strength-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "strength".to_string(),
                diff: -2,
                value: 2,
                description: Some(
                    "Your 'Strength' quality decreased by 2 (new level: 2)".to_string()
                )
            }]
        );

        context.draw_cards();

        let result = context.select_branch("decrease-jewels-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewels".to_string(),
                diff: -2,
                value: 2,
                description: Some("You lost 2 Jewels (new total: 2)".to_string())
            }]
        );
    }

    #[test]
    fn test_quality_change_descriptions_lose() {
        let mut context =
            Context::from_throne_text(include_str!("quality-description-test.throne"));

        context.set_quality("season", 2);
        context.set_quality("strength", 2);
        context.set_quality("jewels", 2);

        context.draw_cards();

        let result = context.select_branch("decrease-season-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "season".to_string(),
                diff: -2,
                value: 0,
                description: Some("The seasons no longer pass".to_string())
            }]
        );

        context.draw_cards();

        let result = context.select_branch("decrease-strength-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "strength".to_string(),
                diff: -2,
                value: 0,
                description: Some(
                    "Your 'Strength' quality decreased by 2 (new level: 0)".to_string()
                )
            }]
        );

        context.draw_cards();

        let result = context.select_branch("decrease-jewels-2");

        context.throne_context.print();

        assert_eq!(
            result.effects,
            vec![BranchResultEffect::QualityChanged {
                quality: "jewels".to_string(),
                diff: -2,
                value: 0,
                description: Some("You lost 2 Jewels (new total: 0)".to_string())
            }]
        );
    }

    #[test]
    fn test_urgency() {
        let mut context = Context::from_throne_text(include_str!("urgency-test.throne"));

        context.draw_cards();

        context.throne_context.print();

        let cards = context.get_cards();
        assert_eq!(cards.len(), 1);
        assert_eq!(cards[0].id, "waterfall".to_string());

        let result = context.select_branch("continue");

        context.throne_context.print();

        assert_eq!(result.id, "passed");

        context.draw_cards();

        context.throne_context.print();

        let cards = context.get_cards();
        assert_eq!(cards.len(), 2);
        assert_eq!(
            cards.iter().map(|c| c.id.clone()).collect::<Vec<_>>(),
            vec!["treasure".to_string(), "treasure2".to_string()]
        );
    }

    #[test]
    fn test_concise_syntax() {
        let actual = expand_concise_syntax(include_str!("concise-test.throne"));

        let expected =
"<<quality-is-thing quality-1
<<quality-level-change-description quality-1 3 `Autumn is upon us`
<<card-title treasure `Treasure for all`
<<card-description treasure `You see a gleaming metal chest`
<<card-required-quality-missing treasure treasure-open
<<card-branch treasure open-the-chest-2
<<branch-title open-the-chest-2 `Open the chest`
<<branch-description open-the-chest-2 `The obvious choice - you\'d be a fool not to take the opportunity.`
<<branch-result open-the-chest-2 you\'re-rich--3
<<result-title you\'re-rich--3 `You\'re rich!`
<<result-description you\'re-rich--3 `The chest is filled with gold and precious jewels.`
<<result-set-quality you\'re-rich--3 treasure-open 1
<<card-branch treasure kick-the-chest-4
<<branch-title kick-the-chest-4 `Kick the chest`
<<branch-description kick-the-chest-4 `It looks very sturdy, but then again you\'re very strong.`
<<branch-required-quality-gt kick-the-chest-4 strength 2
<<card-title angel-falls-5 `Angel Falls`
<<card-description angel-falls-5 `The waterfall is crashing down and your throat is parched.`
<<card-branch angel-falls-5 satisfy-your-thirst-6
<<branch-title satisfy-your-thirst-6 `Satisfy your thirst`
<<branch-description satisfy-your-thirst-6 `The water looks crystal clear, but is it really safe to drink.`";

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_difficulty_probability() {
        let mut context = Context::from_throne_text(include_str!("difficulty-test.throne"));

        context.draw_cards();

        let cards = context.get_cards();
        let branch = &cards[0].branches[0];

        context.throne_context.print();

        assert_eq!(
            branch.result_weights,
            vec![ResultWeight {
                result: "pass".to_string(),
                probability: 1, // 1% is minimum,
                difficulty_quality: Some("strength".to_string()),
            }]
        );

        context.set_quality("strength", 4);
        context.draw_cards();

        let cards = context.get_cards();
        let branch = &cards[0].branches[0];

        context.throne_context.print();

        assert_eq!(
            branch.result_weights,
            vec![ResultWeight {
                result: "pass".to_string(),
                probability: 24,
                difficulty_quality: Some("strength".to_string())
            }]
        );
    }

    #[test]
    fn test_difficulty_distribution() {
        let mut context = Context::from_throne_text(include_str!("difficulty-test.throne"));

        {
            let mut pass_count = 0;
            let mut fail_count = 0;
            for _ in 0..100 {
                context.draw_cards();

                let result = context.select_branch("open-chest");
                if result.id == "pass" {
                    pass_count += 1;
                } else if result.id == "fail" {
                    fail_count += 1;
                }
            }

            assert!(
                pass_count <= 3,
                format!("{} should be <= {}", pass_count, 3)
            );
            assert!(
                fail_count >= 100 - 3,
                format!("{} should be <= {}", fail_count, 100 - 3)
            );
        }

        context.set_quality("strength", 4);

        {
            let count = 500;

            let mut pass_count = 0;
            let mut fail_count = 0;
            for _ in 0..count {
                context.draw_cards();

                let result = context.select_branch("open-chest");
                if result.id == "pass" {
                    pass_count += 1;
                } else if result.id == "fail" {
                    fail_count += 1;
                }
            }

            context.throne_context.print();

            assert!(
                pass_count >= count * 21 / 100,
                format!("{} should be >= {}", pass_count, count * 21 / 100)
            );
            assert!(
                pass_count <= count * 27 / 100,
                format!("{} should be <= {}", pass_count, count * 27 / 100)
            );

            assert!(
                fail_count >= count * (100 - 27) / 100,
                format!("{} should be >= {}", fail_count, count * (100 - 27) / 100)
            );
            assert!(
                fail_count <= count * (100 - 21) / 100,
                format!("{} should be <= {}", fail_count, count * (100 - 21) / 100)
            );
        }
    }

    #[test]
    fn test_set_active_script() {
        let script1 = Script::from_throne_text(
            "
quality local 1
quality global 1

<<quality-title local `Local`

<<quality-title global `Global`
<<quality-is-global global
",
        );

        let script2 = Script::from_throne_text(
            "
quality other 1
",
        );

        let mut context = Context::new();

        context.set_active_script(&script1);

        assert_eq!(
            context.get_qualities(),
            vec![
                Quality {
                    id: "global".to_string(),
                    value: 1,
                    title: Some("Global".to_string()),
                    description: Some("Global: 1".to_string())
                },
                Quality {
                    id: "local".to_string(),
                    value: 1,
                    title: Some("Local".to_string()),
                    description: Some("Local: 1".to_string())
                }
            ]
        );

        context.set_active_script(&script2);

        assert_eq!(
            context.get_qualities(),
            vec![
                Quality {
                    id: "global".to_string(),
                    value: 1,
                    title: Some("Global".to_string()),
                    description: Some("Global: 1".to_string())
                },
                Quality {
                    id: "other".to_string(),
                    value: 1,
                    title: None,
                    description: None
                }
            ]
        );
    }
}
