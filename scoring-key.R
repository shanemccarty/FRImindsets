# =============================================================================
# FRI Peer Mentor Mindsets Guide - COMPOSITE SCORING KEY (for review)
# Source: "2026-27 - Peer Mentor Survey - Pre" v2 (pretest), two Qualtrics exports
#
# Convention:
#   MEAN  composites  -> Likert / frequency / importance / semantic-differential
#   SUM   composites  -> the two constant-sum "budget" blocks (Q6 and Q11)
#
# Column names below are the CURRENT Qualtrics export tags. Three blocks are
# scheduled to be renamed in Qualtrics (see "Survey Export Changes.docx"):
# Q6_* -> Style, Q11_* -> Role (TASK/RELATE). After you re-export with the new
# names, update the references in sections 2 and 7 accordingly.
# =============================================================================

library(tidyverse)
library(readxl)

# -----------------------------------------------------------------------------
# DATA: merge the two Qualtrics exports into one dataset (tidyverse)
# -----------------------------------------------------------------------------
# Both files share an identical 123-column schema, so this is a row-stack
# (32 + 79 = 111 responses). Each Qualtrics .xlsx has THREE header rows:
#   row 1 = export tags (our column names), row 2 = question wording,
#   row 3 = ImportId JSON. We keep row 1 as the names and skip rows 2-3.
# -----------------------------------------------------------------------------
data_dir <- "/Users/smccarty1/Desktop/FRI Mentoring"   # folder holding the exports
files <- c(
  "2026-27 - Peer Mentor Survey - Pre_August 19, 2026_18.56.xlsx",
  "2026-27 - Peer Mentor Survey - Pre_August 19, 2026_18.56-2.xlsx")

read_qualtrics <- function(path) {
  tags <- names(read_excel(path, n_max = 0))       # row 1 = export tags
  read_excel(path, skip = 3, col_names = tags)     # data only (drop 3 header rows)
}

dat <- file.path(data_dir, files) |>
  map(read_qualtrics) |>
  bind_rows() |>                                    # stack the two exports
  mutate(across(EFFECTIVE:Accommodate3, as.numeric))  # item columns -> numeric

# -----------------------------------------------------------------------------
# CLEAN the lookup key: PASSWORD must be unique and non-missing.
#   - Drop rows with no PASSWORD (these 32 rows are entirely empty and cannot be
#     looked up anyway).
#   - For the 13 duplicated passwords (someone who restarted the survey), keep the
#     row with the MOST answered items; break ties by the most recent EndDate.
#     This keeps the fuller attempt when a first try was started but not finished.
#   111 rows -> 66 unique respondents.
# -----------------------------------------------------------------------------
dat <- dat |>
  filter(!is.na(PASSWORD)) |>
  mutate(.n_answered = rowSums(!is.na(across(EFFECTIVE:Accommodate3)))) |>
  arrange(desc(.n_answered), desc(EndDate)) |>       # most complete, then most recent
  distinct(PASSWORD, .keep_all = TRUE) |>             # one row per password
  select(-.n_answered)

# -----------------------------------------------------------------------------
rev6 <- function(x) 7 - x   # reverse-code a 1-6 item: (1+6) - x

scores <- dat |> transmute(

  Password = PASSWORD,

  # ---------------------------------------------------------------------------
  # 1. MENTORING COMPETENCY ASSESSMENT (MCA) - Q5, rated 1-7 (skill)
  #    Fleming et al. (2013), Table 3: six competency submeasures (each the MEAN
  #    of its items, 26 items total) PLUS one overall MCA score.  [CONFIRMED: both]
  #    Overall = MEAN of all 26 items (item-weighted).
  # ---------------------------------------------------------------------------
  MCA_Overall = rowMeans(across(c(
    Effect_Commu1, Effect_Commu2, Effect_Commu3, Effect_Commu4, Effect_Commu5, Effect_Commu6,
    Align_Expect1, Align_Expect2, Align_Expect3, Align_Expect4, Align_Expect5,
    Assess_Under1, Assess_Under2, Assess_Under3,
    Foster_Indepen1, Foster_Indepen2, Foster_Indepen3, Foster_Indepen4, Foster_Indepen5,
    Address_Diver1, Address_Diver2,
    Profess_Devel1, Profess_Devel2, Profess_Devel3, Profess_Devel4, Profess_Devel5))),
  MCA_Communication   = rowMeans(across(c(Effect_Commu1, Effect_Commu2, Effect_Commu3,
                                          Effect_Commu4, Effect_Commu5, Effect_Commu6))),
  MCA_Expectations    = rowMeans(across(c(Align_Expect1, Align_Expect2, Align_Expect3,
                                          Align_Expect4, Align_Expect5))),
  MCA_Understanding   = rowMeans(across(c(Assess_Under1, Assess_Under2, Assess_Under3))),
  MCA_Independence    = rowMeans(across(c(Foster_Indepen1, Foster_Indepen2, Foster_Indepen3,
                                          Foster_Indepen4, Foster_Indepen5))),
  MCA_Diversity       = rowMeans(across(c(Address_Diver1, Address_Diver2))),
  MCA_ProfDevelopment = rowMeans(across(c(Profess_Devel1, Profess_Devel2, Profess_Devel3,
                                          Profess_Devel4, Profess_Devel5))),
  # Chapter 1 "Your Type" typology (wk01): independent support-skill vs
  # structure-skill composites drawn from selected MCA items. Unlike the Q6
  # budget (Support/Structure below), these are not constant-sum, so all four
  # mentoring-style quadrants are reachable.
  MCA_SupportSkill   = rowMeans(across(c(Assess_Under1, Assess_Under2, Assess_Under3, Foster_Indepen2))),
  MCA_StructureSkill = rowMeans(across(c(Align_Expect1, Align_Expect2, Profess_Devel3))),

  # ---------------------------------------------------------------------------
  # 2. MENTORING STYLE - "Style" block (Q6), constant-sum,
  #    budget = 40, scale 1-10.                                 >>> SUM <<<
  #    (Q6_* will be renamed to Style_* in Qualtrics; update refs after re-export.)
  #    Support   = Q6_1 + Q6_2 + Q6_3 + Q6_4
  #    Structure = Q6_5 + Q6_6 + Q6_7 + Q6_8                    [CONFIRMED]
  # ---------------------------------------------------------------------------
  Support   = Q6_1 + Q6_2 + Q6_3 + Q6_4,
  Structure = Q6_5 + Q6_6 + Q6_7 + Q6_8,

  # ---------------------------------------------------------------------------
  # 3. MOTIVATIONAL STYLE (regulatory focus 2x2) - FOCUS, rated 1-7 (emphasis)
  #    Four cell composites, each the MEAN of three items.
  # ---------------------------------------------------------------------------
  Promotion_Academic  = rowMeans(across(c(Pro_Acad1, Pro_Acad2, Pro_Acad3))),
  Promotion_Social    = rowMeans(across(c(Pro_Social1, Pro_Social2, Pro_Social3))),
  Prevention_Academic = rowMeans(across(c(Prev_Acad1, Prev_Acad2, Prev_Acad3))),
  Prevention_Social   = rowMeans(across(c(Prev_Social1, Prev_Social2, Prev_Social3))),

  # ---------------------------------------------------------------------------
  # 4. LEADERSHIP FOCUS (regulatory focus for the team) - LEADFOCUS, rated 1-7
  #    Two composites, each the MEAN of three items.
  # ---------------------------------------------------------------------------
  Lead_Promotion  = rowMeans(across(c(Lead_Promo1, Lead_Promo2, Lead_Promo3))),
  Lead_Prevention = rowMeans(across(c(Lead_Prev1, Lead_Prev2, Lead_Prev3))),

  # ---------------------------------------------------------------------------
  # 5. FEEDBACK ORIENTATION (bipolar) - Q9, 3 semantic-differential items
  #    Export tags Feedback1..Feedback3, coded -3..+3 (Balanced = 0). Verified in
  #    the export (values fall within -3..+3, not 1..7).            [CONFIRMED]
  #    MEAN of the three -> ranges -3..+3, with 0 = balanced.
  # ---------------------------------------------------------------------------
  Feedback_Orientation = rowMeans(across(c(Feedback1, Feedback2, Feedback3))),

  # ---------------------------------------------------------------------------
  # 6. PEOPLE ORIENTATION / GROWTH MINDSET - Q10, rated 1-6 (agreement)
  #    MEAN of 8 items; reverse-code the four _R items first.
  # ---------------------------------------------------------------------------
  Growth_Mindset = rowMeans(cbind(
    GrowthMind3, GrowthMind5, GrowthMind7, GrowthMind8,       # keyed normally
    rev6(GrowthMind1_R), rev6(GrowthMind2_R),                 # reverse-coded
    rev6(GrowthMind4_R), rev6(GrowthMind6_R))),

  # ---------------------------------------------------------------------------
  # 7. LEADERSHIP ROLE - "Role" block (Q11), Morgeson functions, constant-sum,
  #    budget = 50, scale 1-10.                                 >>> SUM <<<
  #    Current export tags are Q11_n (TASK/RELATE variable naming not yet applied).
  #    Crosswalk  (item -> intended name -> group):
  #      Q11_1  Help them plan ............. TASK1   -> Task
  #      Q11_4  Keep track of due dates .... TASK2   -> Task
  #      Q11_6  Train the team ............. TASK3   -> Task
  #      Q11_7  Provide feedback .......... TASK4   -> Task
  #      Q11_5  Monitor the group chat .... RELATE1 -> Relational
  #      Q11_9  Motivate the team ......... RELATE2 -> Relational
  #      Q11_10 Encourage self-manage ..... RELATE3 -> Relational
  #      Q11_11 Resolve team conflict ..... RELATE4 -> Relational
  #      Q11_8  Evaluate performance ...... RELATE5 -> Relational
  #      Q11_12 Build friendships ......... RELATE6 -> Relational
  # ---------------------------------------------------------------------------
  Task       = Q11_1 + Q11_4 + Q11_6 + Q11_7,
  Relational = Q11_5 + Q11_8 + Q11_9 + Q11_10 + Q11_11 + Q11_12,

  # ---------------------------------------------------------------------------
  # 8. COMMUNICATION STYLE - Q12, rated 1-4 (frequency)
  #    Three subscales, each the MEAN of its items (2 + 2 + 3 = 7).  [CONFIRMED]
  # ---------------------------------------------------------------------------
  Comm_SelfConnection = rowMeans(across(c(Selfconnect1, Selfconnect2))),
  Comm_SelfExpression = rowMeans(across(c(Selfexpress1, Selfexpress2))),
  Comm_Listening      = rowMeans(across(c(Listen1, Listen2, Listen3))),

  # ---------------------------------------------------------------------------
  # 9. INCLUSIVE MENTORING (CDA-R/E) - Q13, rated 1-6 (importance)
  #    Two composites, each the MEAN of three items.
  # ---------------------------------------------------------------------------
  Inclusive_Attitudes = rowMeans(across(c(Attitudes1, Attitudes2, Attitudes3))),
  Inclusive_Behaviors = rowMeans(across(c(Behaviors1, Behaviors2, Behaviors3))),

  # ---------------------------------------------------------------------------
  # 10. CONFLICT STYLE (DUTCH) - Q14, rated 1-5
  #     Five styles, each the MEAN of three items.
  # ---------------------------------------------------------------------------
  Conflict_Competing     = rowMeans(across(c(Competing1, Competing2, Competing3))),
  Conflict_Collaborating = rowMeans(across(c(Collaborating1, Collaborating2, Collaborating3))),
  Conflict_Compromising  = rowMeans(across(c(Compromising1, Compromising2, Compromising3))),
  Conflict_Avoiding      = rowMeans(across(c(Avoiding1, Avoiding2, Avoiding3))),
  Conflict_Accommodating = rowMeans(across(c(Accommodate1, Accommodate2, Accommodate3)))
)

# =============================================================================
# NOT composites (single items) - kept for the descriptive plots/tables you
# flagged as future tasks in the survey:
#   EFFECTIVE : 1-5 importance of becoming more effective   -> histogram
#   STREAM    : 1-12 which research stream                  -> count table
# =============================================================================

# =============================================================================
# EXPORT for the guide (data/survey_pre.csv, read by _setup.R).
# De-identified: PASSWORD + item responses only (no email / IP / name / location).
# Keeps only respondents who answered at least one item, which drops the single
# password-only row -> 65 respondents with scores.
# =============================================================================
dat |>
  filter(rowSums(!is.na(across(Effect_Commu1:Accommodate3))) > 0) |>  # answered >=1 mindset item
  select(PASSWORD, EFFECTIVE:Accommodate3) |>
  write_csv("data/survey_pre.csv")
