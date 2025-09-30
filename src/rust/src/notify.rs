use std::collections::HashMap;

use extendr_api::{deserializer::from_robj, prelude::*, serializer::to_robj, Robj};
use ghqctoolkit::GitHubReader;
use octocrab::models::Milestone;

use crate::utils::get_rt;

pub fn get_multiple_milestone_issues_impl(
    git_info: &impl GitHubReader,
    milestones_robj: &Robj,
) -> Result<Robj> {
    let rt = get_rt();

    // Deserialize milestones from R
    let milestones: Vec<Milestone> = from_robj(milestones_robj)?;

    // Create futures for parallel execution
    let futures: Vec<_> = milestones
        .iter()
        .map(|milestone| {
            let milestone_title = milestone.title.clone();
            async move {
                let issues = git_info.get_milestone_issues(milestone).await;
                (milestone_title, issues)
            }
        })
        .collect();

    // Execute all futures in parallel
    let results = rt.block_on(futures::future::join_all(futures));

    // Build the result hashmap, only including successful results
    let mut milestone_issues = HashMap::new();

    for (milestone_title, issues_result) in results {
        match issues_result {
            Ok(issues) => {
                // Convert issues to R objects
                let r_issues: Result<Vec<Robj>> = issues.iter().map(to_robj).collect();
                match r_issues {
                    Ok(issues_vec) => {
                        milestone_issues.insert(milestone_title, issues_vec);
                    }
                    Err(e) => {
                        log::warn!("Failed to convert issues for milestone {}: {}", milestone_title, e);
                        // Don't insert entry for this milestone on conversion error
                    }
                }
            }
            Err(e) => {
                log::warn!("Failed to get issues for milestone {}: {}", milestone_title, e);
                // Don't insert entry for this milestone on API error
            }
        }
    }

    // Convert HashMap to R named list
    let mut names = Vec::new();
    let mut values: Vec<Robj> = Vec::new();

    for (name, issues) in milestone_issues {
        names.push(name);
        let issues_list = List::from_values(issues);
        values.push(issues_list.into());
    }

    let result_list = List::from_names_and_values(names, values)?;
    Ok(result_list.into())
}