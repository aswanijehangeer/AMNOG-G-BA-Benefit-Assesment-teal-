# AMNOG / G-BA benefit assessment demo (teal app)

# Mimics analyses from an AMNOG dossier (Module 4) submitted to the German
# Federal Joint Committee (G-BA): a new drug ("Intervention") is compared with
# the appropriate comparator therapy ("ZVT").

# File layout:
#   1. Packages
#   2. Simulated trial data (CDISC ADaM-style: ADSL, ADTTE, ADAE)
#   3. Custom teal module: adverse events (safety)
#   4. Custom teal module: overall survival (efficacy)
#   5. Assemble and launch the app

# All data are FAKE. Results have no clinical meaning.

# install.packages(c("teal", "survival", "ggplot2", "dplyr"))

# 1. Packages

library(teal) # framework: filter panel + modules + reproducible code
library(survival) # Surv(), survfit() (Kaplan-Meier), coxph() (hazard ratios)
library(ggplot2) # plotting


# 2. Simulated trial data

# teal_data() is a container that stores both the datasets AND the code that
# created them. Code run inside within() is recorded, so teal can later show
# users exactly how the data were produced (reproducibility).

data <- teal_data()
data <- within(data, {
  # Fixed seed -> same fake data every time the app starts
  set.seed(4817)
  n <- 400

  # --- ADSL: Subject-Level Analysis Dataset (one row per patient)
  ADSL <- data.frame(
    STUDYID = "AMNOG-DEMO",
    USUBJID = sprintf("SUBJ-%03d", seq_len(n)), # unique patient ID
    # ZVT is the first factor level, so it is the reference group in models
    # and all hazard ratios read as "Intervention vs. ZVT"
    ARM = factor(
      sample(c("Intervention", "Comparator (ZVT)"), n, TRUE),
      levels = c("Comparator (ZVT)", "Intervention")
    ),
    # Subgroup variables used later for subgroup analyses and filtering
    SEX = sample(c("F", "M"), n, TRUE),
    AGEGR = sample(c("<65", ">=65"), n, TRUE, prob = c(0.6, 0.4)),
    REGION = sample(c("Europe", "North America", "Asia"), n, TRUE)
  )

  # --- ADTTE: Time-To-Event dataset (overall survival)
  # Death times come from an exponential distribution. The Intervention gets a
  # lower monthly hazard (0.05 vs 0.075), i.e. the "true" HR is ~0.67.
  rate <- ifelse(ADSL$ARM == "Intervention", 0.05, 0.075)
  t_event <- rexp(n, rate)
  # Censoring time: the patient leaves follow-up between month 6 and 36
  t_cens <- runif(n, 6, 36)
  ADTTE <- data.frame(
    STUDYID = "AMNOG-DEMO",
    USUBJID = ADSL$USUBJID,
    PARAMCD = "OS",
    PARAM = "Overall survival",
    # Observed time = whichever comes first: death or censoring
    AVAL = round(pmin(t_event, t_cens), 2),
    # CDISC convention: CNSR = 1 means censored, 0 means event (death)
    CNSR = as.integer(t_event > t_cens)
  )

  # --- ADAE: Adverse Events dataset (one row per event) ----------
  # Small dictionary of MedDRA-style terms:
  #   AEBODSYS = System Organ Class (SOC), AEDECOD = Preferred Term (PT)
  ae_terms <- data.frame(
    AEBODSYS = c(
      "Gastrointestinal disorders",
      "Gastrointestinal disorders",
      "Blood and lymphatic system disorders",
      "Blood and lymphatic system disorders",
      "General disorders",
      "Infections and infestations",
      "Skin and subcutaneous tissue disorders",
      "Nervous system disorders"
    ),
    AEDECOD = c(
      "Nausea",
      "Diarrhoea",
      "Neutropenia",
      "Anaemia",
      "Fatigue",
      "Pneumonia",
      "Rash",
      "Headache"
    )
  )
  # Number of AEs per patient (Poisson); Intervention has slightly more
  n_ae <- rpois(n, ifelse(ADSL$ARM == "Intervention", 2.2, 1.8))
  # Repeat each patient's row index once per AE they have
  idx <- rep(seq_len(n), n_ae)
  # Randomly assign a term and a CTCAE severity grade (1 = mild ... 5 = death)
  term <- ae_terms[sample(nrow(ae_terms), length(idx), TRUE), ]
  grade <- sample(1:5, length(idx), TRUE, prob = c(0.45, 0.3, 0.17, 0.06, 0.02))
  ADAE <- data.frame(
    STUDYID = "AMNOG-DEMO",
    USUBJID = ADSL$USUBJID[idx],
    AEBODSYS = term$AEBODSYS,
    AEDECOD = term$AEDECOD,
    AETOXGR = grade,
    # Serious AE flag: only grade >= 3 events can be serious (60% of them)
    AESER = ifelse(grade >= 3 & runif(length(idx)) < 0.6, "Y", "N"),
    # Action taken: 30% of grade >= 3 events lead to treatment stop
    AEACN = ifelse(
      grade >= 3 & runif(length(idx)) < 0.3,
      "DRUG WITHDRAWN",
      "DOSE NOT CHANGED"
    )
  )

  # Remove helper objects so only the three datasets remain in teal_data
  rm(n, rate, t_event, t_cens, ae_terms, n_ae, idx, term, grade)
})

# Join keys tell teal how the tables relate (via STUDYID + USUBJID).
# Effect: filtering patients in ADSL (e.g. SEX == "F") automatically filters
# their rows in ADTTE and ADAE too.
join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADTTE", "ADAE")]


# 3. Custom teal module: Adverse events (G-BA safety tables)
#
# A teal module = a label + a UI function + a server function.
#   - ui:     what appears in the tab (headings, inputs, placeholders)
#   - server: computes outputs; receives `data`, a reactive holding the
#             FILTERED datasets (reacts automatically to the filter panel)

ae_module <- function(label = "Adverse events (G-BA)") {
  module(
    label = label,
    # Only these datasets are passed to the module / shown in its filter panel
    datanames = c("ADSL", "ADAE"),

    ui = function(id) {
      # NS() namespaces input/output IDs so several modules can coexist
      ns <- NS(id)
      tagList(
        h4("Overview of adverse events (patients with >= 1 event)"),
        tableOutput(ns("overview")),
        h4("Severe AEs (CTCAE grade >= 3) by SOC / PT"),
        # User-adjustable threshold to hide rare terms
        numericInput(
          ns("min_pct"),
          "Show terms with incidence >= x% in any arm",
          value = 1,
          min = 0,
          max = 100,
          step = 1,
          width = "300px"
        ),
        tableOutput(ns("grade3")),
        helpText(
          "RR = relative risk (Intervention vs. ZVT), Wald 95% CI; p from Fisher's exact test."
        )
      )
    },

    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        # Column order in the output tables
        arms <- c("Intervention", "Comparator (ZVT)")

        # Helper: build ONE table row for an endpoint.
        #   label = row name shown in the table
        #   ids   = patient IDs that had the event (may contain duplicates)
        #   adsl  = filtered patient list (denominator)
        # Counts PATIENTS with >= 1 event, not number of events.
        rr_row <- function(label, ids, adsl) {
          # Denominators: patients per arm
          n_tot <- table(factor(adsl$ARM, levels = arms))
          # Numerators: patients per arm with the event
          has <- adsl$USUBJID %in% ids
          n_ev <- table(factor(adsl$ARM[has], levels = arms))
          # 2x2 table cells: a/b = Intervention events/total, c/d = ZVT
          a <- n_ev[[1]]
          b <- n_tot[[1]]
          c <- n_ev[[2]]
          d <- n_tot[[2]]
          fmt <- function(x, N) sprintf("%d (%.1f)", x, 100 * x / N)

          # Relative risk = risk(Intervention) / risk(ZVT).
          # 95% CI computed on log scale (Wald method). Undefined when an arm
          # has 0 events, so report "NC" (not calculable).
          if (a > 0 && c > 0) {
            rr <- (a / b) / (c / d)
            se <- sqrt(1 / a - 1 / b + 1 / c - 1 / d)
            rr_txt <- sprintf(
              "%.2f [%.2f; %.2f]",
              rr,
              rr * exp(-1.96 * se),
              rr * exp(1.96 * se)
            )
          } else {
            rr_txt <- "NC"
          }

          # Fisher's exact test on the 2x2 table (event yes/no x arm)
          p <- fisher.test(matrix(c(a, b - a, c, d - c), 2))$p.value

          data.frame(
            Endpoint = label,
            `Intervention n (%)` = fmt(a, b),
            `ZVT n (%)` = fmt(c, d),
            `RR [95% CI]` = rr_txt,
            p = format.pval(p, digits = 2, eps = 0.001),
            check.names = FALSE # keep column names with spaces/brackets
          )
        }

        # Reactive: re-runs whenever the filter panel changes.
        # Keeps only AEs belonging to patients that survive the ADSL filter.
        merged <- reactive({
          d <- data()
          adsl <- d[["ADSL"]]
          list(
            adsl = adsl,
            adae = d[["ADAE"]][d[["ADAE"]]$USUBJID %in% adsl$USUBJID, ]
          )
        })

        # Table 1: standard G-BA AE overview categories
        output$overview <- renderTable({
          m <- merged()
          # Show a friendly message instead of an error if a filter removes an arm
          validate(need(length(unique(m$adsl$ARM)) == 2, "Both arms needed."))
          ae <- m$adae
          rbind(
            rr_row("Any AE", ae$USUBJID, m$adsl),
            rr_row("Serious AE (SAE)", ae$USUBJID[ae$AESER == "Y"], m$adsl),
            rr_row(
              "Severe AE (CTCAE grade >= 3)",
              ae$USUBJID[ae$AETOXGR >= 3],
              m$adsl
            ),
            rr_row(
              "Discontinuation due to AE",
              ae$USUBJID[ae$AEACN == "DRUG WITHDRAWN"],
              m$adsl
            )
          )
        })

        # Table 2: grade >= 3 AEs, organ class (SOC) rows with indented
        # preferred terms (PT) underneath
        output$grade3 <- renderTable({
          m <- merged()
          validate(need(length(unique(m$adsl$ARM)) == 2, "Both arms needed."))
          ae <- m$adae[m$adae$AETOXGR >= 3, ]
          validate(need(nrow(ae) > 0, "No grade >= 3 events."))
          n_tot <- table(factor(m$adsl$ARM, levels = arms))

          # TRUE if the incidence in at least one arm reaches the threshold
          # chosen by the user (input$min_pct)
          keep_term <- function(ids) {
            arm_ev <- table(factor(
              m$adsl$ARM[m$adsl$USUBJID %in% ids],
              levels = arms
            ))
            any(100 * arm_ev / n_tot >= input$min_pct)
          }

          # Build rows: SOC first, then its PTs (indented with spaces)
          rows <- list()
          for (soc in sort(unique(ae$AEBODSYS))) {
            ae_soc <- ae[ae$AEBODSYS == soc, ]
            if (!keep_term(ae_soc$USUBJID)) {
              next # skip whole SOC if below threshold
            }
            rows <- c(rows, list(rr_row(soc, ae_soc$USUBJID, m$adsl)))
            for (pt in sort(unique(ae_soc$AEDECOD))) {
              ids <- ae_soc$USUBJID[ae_soc$AEDECOD == pt]
              if (keep_term(ids)) {
                rows <- c(rows, list(rr_row(paste0("    ", pt), ids, m$adsl)))
              }
            }
          }
          validate(need(length(rows) > 0, "No terms above threshold."))
          # Stack the list of one-row data frames into one table
          do.call(rbind, rows)
        })
      })
    }
  )
}


# 4. Custom teal module: Overall survival (efficacy)
#
# Shows a Kaplan-Meier curve and a subgroup table of hazard ratios, the core
# of an AMNOG efficacy assessment.

amnog_module <- function(label = "AMNOG benefit assessment") {
  module(
    label = label,
    datanames = c("ADSL", "ADTTE"),

    ui = function(id) {
      ns <- NS(id)
      tagList(
        h4("Overall survival: Kaplan-Meier"),
        plotOutput(ns("km"), height = "400px"),
        h4("Subgroup analysis (Cox HR, Intervention vs. ZVT)"),
        tableOutput(ns("subgroups")),
        helpText(
          "Effect modification flagged per G-BA convention when interaction p < 0.05."
        )
      )
    },

    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        # Combine survival times with patient characteristics (arm, sex, ...)
        # so we can model by arm and split by subgroup. Re-runs on filtering.
        adtte <- reactive({
          d <- data()
          merge(d[["ADTTE"]], d[["ADSL"]], by = c("STUDYID", "USUBJID"))
        })

        # Kaplan-Meier plot
        output$km <- renderPlot({
          df <- adtte()
          validate(need(nrow(df) > 10, "Not enough data after filtering."))
          # Surv(time, event): event indicator is 1 - CNSR (1 = death)
          fit <- survfit(Surv(AVAL, 1 - CNSR) ~ ARM, data = df)
          # Extract curve points into a data frame for ggplot
          s <- summary(fit, censored = TRUE)
          km <- data.frame(
            time = s$time,
            surv = s$surv,
            ARM = sub("ARM=", "", s$strata) # "ARM=Intervention" -> "Intervention"
          )
          # Step function = the classic KM curve shape
          ggplot(km, aes(time, surv, colour = ARM)) +
            geom_step() +
            labs(x = "Months", y = "Survival probability", colour = NULL) +
            theme_minimal() +
            theme(legend.position = "bottom")
        })

        # Subgroup hazard ratio table
        output$subgroups <- renderTable(
          {
            df <- adtte()
            validate(need(nrow(df) > 10, "Not enough data after filtering."))

            # Helper: Cox model HR (Intervention vs. ZVT) for one data subset.
            # HR < 1 means lower risk of death with the Intervention.
            hr_row <- function(d, label) {
              # Skip if a subset lacks an arm or has < 5 deaths (unstable)
              if (length(unique(d$ARM)) < 2 || sum(1 - d$CNSR) < 5) {
                return(data.frame(
                  Subgroup = label,
                  N = nrow(d),
                  HR = NA,
                  `95% CI` = NA,
                  p = NA,
                  check.names = FALSE
                ))
              }
              # conf.int columns: HR, 1/HR, lower 95%, upper 95%
              ci <- summary(coxph(
                Surv(AVAL, 1 - CNSR) ~ ARM,
                data = d
              ))$conf.int
              # Column 5 of coefficients = Wald test p-value
              p <- summary(coxph(
                Surv(AVAL, 1 - CNSR) ~ ARM,
                data = d
              ))$coefficients[, 5]
              data.frame(
                Subgroup = label,
                N = nrow(d),
                HR = sprintf("%.2f", ci[1]),
                `95% CI` = sprintf("[%.2f; %.2f]", ci[3], ci[4]),
                p = format.pval(p, digits = 2, eps = 0.001),
                check.names = FALSE
              )
            }

            # Helper: interaction test. Does the treatment effect DIFFER
            # between levels of `var` (e.g. men vs. women)?
            # Compares a model without interaction (ARM + var) to one with it
            # (ARM * var) via a likelihood-ratio test. G-BA treats p < 0.05 as
            # a signal of effect modification.
            interaction_p <- function(var) {
              if (length(unique(df[[var]])) < 2) {
                return(NA) # filtered down to one level -> nothing to compare
              }
              f0 <- coxph(
                as.formula(paste("Surv(AVAL, 1 - CNSR) ~ ARM +", var)),
                data = df
              )
              f1 <- coxph(
                as.formula(paste("Surv(AVAL, 1 - CNSR) ~ ARM *", var)),
                data = df
              )
              anova(f0, f1)[2, "Pr(>|Chi|)"]
            }

            # Build table: "Overall" row, then for each subgroup variable a
            # header row (with interaction p) followed by one row per level
            rows <- list(hr_row(df, "Overall"))
            for (v in c("SEX", "AGEGR", "REGION")) {
              ip <- interaction_p(v)
              rows <- c(
                rows,
                list(data.frame(
                  Subgroup = sprintf(
                    "%s (interaction p = %s)",
                    v,
                    ifelse(
                      is.na(ip),
                      "NA",
                      format.pval(ip, digits = 2, eps = 0.001)
                    )
                  ),
                  N = NA,
                  HR = NA,
                  `95% CI` = NA,
                  p = NA,
                  check.names = FALSE
                ))
              )
              for (lv in sort(unique(df[[v]]))) {
                rows <- c(
                  rows,
                  list(hr_row(df[df[[v]] == lv, ], paste0("  ", lv)))
                )
              }
            }
            do.call(rbind, rows)
          },
          na = "" # show empty cells instead of "NA" in header rows
        )
      })
    }
  )
}


# 5. Assemble and launch the app
#
# init() combines the data and the modules into one app. Each module becomes a
# tab; teal adds the filter panel and "Show R code" features automatically.

app <- init(
  data = data,
  modules = modules(
    amnog_module(),
    ae_module(),
    # Built-in teal module for browsing the raw datasets
    example_module(label = "Data preview")
  ),
  title = build_app_title("AMNOG / G-BA demo (simulated)")
)

# Start the Shiny app (app$ui = layout, app$server = logic)
shinyApp(app$ui, app$server)
