# Manuscript vs. Code Discrepancies
# GISP CEA MS revisions for Lancet RHA
# Reviewed: 2026-05-05

---

## 1. Fitness Costs (Medium)

**Manuscript (Methods):** "There are no fitness costs for resistance in model, as fitness costs for AMR gonorrhea appear to be low."

**Code (`src/msmOnlyModel/Indiv.java:248–261`):** Fitness cost logic IS implemented — `fitnessCostA` and `fitnessCostB` parameters are applied to reduce the weekly transmission probability.

**Action needed:** Verify whether `fitnessCostA` and `fitnessCostB` are set to 0 in all calibrated runs. If so, the statement is accurate in practice but should be clarified (e.g., "parameters for fitness costs were fixed at zero"). If not, the manuscript statement is incorrect.

---

## 2. Screening Interval Distribution (High)

**Manuscript ODD (§S2.7):** "a value is drawn from a **normal distribution** which has a mean of `screenInterval` and a standard deviation of `screenInterval/10`"

**Code (`src/msmOnlyModel/Screener.java:38`):**
```java
GammaDistribution screenIntervalDist = randomHelper.getCMGammaDistribution("screeningIntervalMSMGamma");
```

A **Gamma distribution** is used, not a Normal distribution.

**Action needed:** Update the ODD to say "Gamma distribution" and describe its parameterization (mean and variance), or change the code to match the ODD description.

---

## 3. Screening Initial Value (High)

**Manuscript ODD (§S2.7):** "their list of screenings begins with a value chosen according to a **uniform distribution**, between 0 and the parameter `screenInterval`"

**Code (`src/msmOnlyModel/Screener.java:43`):**
```java
screenings.add((int) screenIntervalDist.sample());
```

The first value is also drawn from the Gamma distribution — no uniform distribution is used for initialization.

**Action needed:** Update the ODD to accurately describe how the first screening time is generated, or update the code to match.

---

## 4. Number of Unique Trajectories — Internal Inconsistency (Medium)

**Manuscript §S1.3 (calibration section):** "This resampling step resulted in **337** unique trajectories."

**Manuscript tables (Table 1, Table S7) and validation section:** "Presented are the means of **380** unique trajectories."

Both numbers appear in the same manuscript and are inconsistent.

**Action needed:** Determine the correct number of unique trajectories and update all instances consistently throughout the manuscript (including §S1.3, tables, figure captions, and the validation section).

---

## 5. Abstaining Attribute Described but Commented Out (Low)

**Manuscript ODD (§S2.2):** "Individuals had a Boolean attribute indicating whether they were currently **abstaining from sexual activity**."

**Code (`src/msmOnlyModel/Indiv.java`):** The abstaining attribute and all related methods are commented out throughout the file.

**Action needed:** Either remove the reference to abstaining from the ODD, or note that it was a planned feature not implemented in this version.

---

## 6. Simulation Length / Timeline Inconsistency (Medium)

**Manuscript (multiple locations):**
- Abstract/model summary: "25-year period"
- Methods calibration section: "trajectories simulated over **ten years** (5-year burn-in + 5-year calibration)"
- Results: "years **5–25** in Figure 2" (implying a 25-year total simulation, with outcomes from years 5–25 = 20 years)

**Code/ODD:** `endTime = 1560` timesteps = **30 years**; ODD states "Simulations run for up to 1560 timesteps, i.e., representing 30 years"

The calibration parameter sweep runs for 10 years; the projection simulations run longer (up to 30 years per the ODD). Outcomes are reported from years 5–25 (20 years). The manuscript inconsistently describes the total simulation length as 25 vs. 30 years in different sections.

**Action needed:** Clarify and standardize the timeline description across the manuscript. Specify explicitly: (a) how long the calibration sweep ran, (b) how long the projection simulations ran, and (c) what period outcomes are drawn from.

---

## 7. GISP Surveillance Sampling: "First 25" vs. Unordered (Low–Medium)

**Manuscript ODD (§S2.2):** "The **first** 25 detected cases during each four-week period undergo drug susceptibility testing"

**Code (`src/msmOnlyModel/SurveillanceProgram.java:150–153`):**
```java
sample = detected.stream().unordered()
        .limit(amountToTest)
        .collect(Collectors.toList());
```

`.unordered().limit()` does not guarantee chronological order — any 25 cases may be selected, not necessarily the first 25 in time.

**Action needed:** Either update the ODD to say "up to 25 randomly selected detected cases" (which better matches the code), or change the code to sort by detection time and take the first 25.

---

## 8. Surveillance Interval: "Four-week" vs. 4.333 weeks (Low)

**Manuscript ODD (§S2.2):** "each **four-week period**"

**Code (`src/msmOnlyModel/SurveillanceProgram.java:80`):**
```java
@ScheduledMethod(start = 261, interval = 4.333, priority = 1)
```

4.333 weeks = 52/12 = one calendar month, not exactly 4 weeks.

**Action needed:** Update the ODD to say "monthly" or "approximately every 4.3 weeks (one calendar month)" to match the code, or note that the interval approximates GISP's monthly reporting cycle.

---

## Summary

| # | Location in Code | Manuscript claim | Code behavior | Priority |
|---|-----------------|-----------------|---------------|----------|
| 1 | `Indiv.java:248–261` | No fitness costs | Fitness cost params implemented | Medium |
| 2 | `Screener.java:38` | Normal dist for screening intervals | Gamma distribution | High |
| 3 | `Screener.java:43` | Uniform dist for first screening | Gamma sample | High |
| 4 | Tables vs §S1.3 | 337 unique trajectories | 380 in tables/validation | Medium |
| 5 | `Indiv.java` | Abstaining attribute exists | Commented out | Low |
| 6 | ODD vs Methods | 25-year simulation | 1560 ticks = 30 years | Medium |
| 7 | `SurveillanceProgram.java:150` | "First 25" detected cases | Unordered `.limit()` | Low–Medium |
| 8 | `SurveillanceProgram.java:80` | 4-week intervals | 4.333-week intervals | Low |
