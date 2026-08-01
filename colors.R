# Construct color palette for the FRI Peer Mentor Mindsets Guide.
#
# Sourced by _setup.R, so every chapter has it. Use these names rather than pasting
# hex codes into chapters: a construct keeps the same color everywhere it appears.
#
# Design rules this palette follows:
#   * Paired constructs sit opposite each other on the color wheel (LCh hue ~180 apart)
#     and share a lightness, so neither pole looks more important than the other.
#   * Every color used for TEXT clears WCAG AA (4.5:1) on white.
#   * Each construct has three weights: base (text, points, bars), mid (emphasis fills),
#     light (backgrounds, quadrant shading).
#
# Brand green stays reserved for UI chrome (headings, links, sidebar) so it never
# competes with a construct color. See theme.scss.

brand_green   <- "#005A43"   # Binghamton green (PMS 342)
brand_accent  <- "#00805D"
neutral_grey  <- "#8A8A8A"
neutral_dark  <- "#333333"

# ---- Construct base colors --------------------------------------------------
# Support / Structure: teal vs red-orange, 178 degrees apart, L = 48.1 / 48.8.
# Promotion / Prevention: soft blue vs deep indigo, specified by the course author.
construct_col <- c(
  Support    = "#007F85",   # teal
  Structure  = "#C0522A",   # red-orange (opposite Support on the wheel)
  Promotion  = "#8297CE",   # soft blue, as specified
  Prevention = "#453A98"    # deep indigo, as specified
)

# Text-safe variants. #8297CE is a fill color: at 2.89:1 on white it fails AA for
# body text, so anything set in type uses the darker Promotion here instead.
construct_text <- c(
  Support    = "#007F85",   # 4.80:1
  Structure  = "#C0522A",   # 4.69:1
  Promotion  = "#5A71A8",   # 4.81:1  (same hue as #8297CE, darkened for legibility)
  Prevention = "#453A98"    # 9.08:1
)

# Mid weight: emphasis fills, bar fills that still need to read as the construct.
construct_mid <- c(
  Support    = "#B1D9DC",
  Structure  = "#EBCAC5",
  Promotion  = "#C9D1EA",
  Prevention = "#D1CFEB"
)

# Light weight: panel/quadrant backgrounds behind data ink.
construct_light <- c(
  Support    = "#D4EDEF",
  Structure  = "#F9E3E0",
  Promotion  = "#E3E8F8",
  Prevention = "#E8E6F8"
)

# ---- Aliases ----------------------------------------------------------------
# Chapters name the same underlying construct differently (Week 3 leadership framing,
# Week 7 SPLIT subscales). Map those labels onto the four base constructs so the
# color follows the construct, not the wording.
construct_alias <- c(
  `Promotion Leadership`  = "Promotion",
  `Prevention Leadership` = "Prevention",
  `Promotion x Academic`  = "Promotion",
  `Promotion x Social`    = "Promotion",
  `Prevention x Academic` = "Prevention",
  `Prevention x Social`   = "Prevention",
  `Task leadership`       = "Structure",   # task focus is the structure pole
  `Relation leadership`   = "Support"      # relational focus is the support pole
)

# Look up a construct color by label, resolving aliases.
# weight: "text" (default), "base", "mid", or "light".
construct_color <- function(label, weight = c("text", "base", "mid", "light")) {
  weight <- match.arg(weight)
  key <- ifelse(label %in% names(construct_alias), construct_alias[label], label)
  pal <- switch(weight, text = construct_text, base = construct_col,
                mid = construct_mid, light = construct_light)
  out <- unname(pal[key])
  # Unmapped constructs (curiosity facets, MCA competencies) fall back to brand green
  # until they are assigned their own colors.
  ifelse(is.na(out), brand_green, out)
}

# Inline span for naming a construct in chapter prose, e.g.
#   `r construct_span("Support")` blends warmth and responsiveness.
construct_span <- function(label, text = label) {
  sprintf('[**%s**]{style="color:%s;"}', text, construct_color(label, "text"))
}

# ---- Week 9 conflict styles -------------------------------------------------
# Five styles, so they need their own family. Competing and Collaborating reuse the
# Structure/Support pair because they are the same assertive/cooperative poles.
# Avoiding and Accommodating get hues held clear of Promotion (255) and Prevention
# (268) so Week 9 is never mistaken for Weeks 2 and 3: olive (H 75) and plum (H 330)
# sit more than 50 degrees from every other palette hue. All clear WCAG AA on white.
conflict_col <- c(
  Competing     = "#C0522A",   # = Structure red-orange (assertive, uncooperative)
  Collaborating = "#007F85",   # = Support teal (assertive, cooperative)
  Compromising  = "#6B6B6B",   # neutral grey: intermediate on both dimensions
  Avoiding      = "#817300",   # olive,  4.79:1
  Accommodating = "#AE518E"    # plum,   4.81:1
)

# ---- Week 4 feedback orientation (bipolar) ----------------------------------
# One bipolar dimension. Its own pair, held clear of every other palette hue so
# Feedback is not mistaken for another chapter: green (H120) and violet (H300)
# sit >=30 degrees from all other hues and are 180 apart. Both clear WCAG AA.
feedback_col <- c(
  Supportive = "#477F2B",   # green,  4.84:1
  Corrective = "#985BA3"    # violet, 4.82:1
)
