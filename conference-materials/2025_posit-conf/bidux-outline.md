# Title updates
* **New title:** *Death by Dropdown? Use Behavioral Science To Engineer Insightful Shiny Apps*

---

# Opening

* **Credibility:** "I'm Jeremy, applied social psychologist (PhD) turned full-stack developer."
  * "As a behavioral scientist in the tech world, I've seen firsthand how a dash of psychology can rescue users from 'death by dropdown.'"
* **Hook:**
  * The overwhelmed user: convoluted Shiny dashboard (too many filters, tabs, and choices) 
  * "Opened the app, saw 18 distinct filters across 6 separate tabs, and immediately closed my laptop."
* **Governing Idea:**
  * By grounding your Shiny apps in behavioral science (X1), through the Behavior Insight Design (BID) framework (X2) and the {bidux} R package (X3), you can build user-centered applications (Y) that improve user experience and decision-making (Z)
  * X1="behavioral science theories/concepts"
  * X2="behavior insight design framework"
  * X3="{bidux} R package"
  * Y="shiny developers can design user centered applications"
  * Z="improve user experience and decision making"

---

# BID Framework

* Display the 5 BID stages graphic, and explain that I'll use these stages as the "chapters" of the talk
* Analogy: Consider a simple analogy to make the framework stick. For example, liken building a dashboard to leading a museum tour:
  1. Notice = picking which exhibits confuse visitors.
  2. Interpret = understanding what stories the visitors seek.
  3. Structure = arranging exhibits so the path feels natural.
  4. Anticipate = placing signs where visitors might get lost (guiding biases).
  5. Validate = ending the tour with a memorable summary in the gift shop (takeaway).
* Differentiate BID: Highlight how BID isn't just generic UX advice but "behavioral science integrated into development workflow". For instance:
  * "We stand on UX and data-viz giants' shoulders, but BID's key differentiator is weaving evidence-based psychology into each step of app design.
  * It's like having a behavioral scientist in the console with you as you code."

---

# Chapters (BID stages)

### 1) Notice: identify friction

* **Goal:** Name the *real* problem users face.
* **Example placeholder:** [Before screenshot] + 1–2 pain bullets (choice overload, slow path to insight).
* **{bidux} angle:** `bid_notice()` → log problem/evidence; (optional) telemetry-driven flags (unused inputs, errors).
* **Analogy:** "Spot the crowded hallway before redesigning the building."
* **Takeaway:** Define the problem crisply before touching layout.

### 2) Interpret: clarify the question & story

* **Goal:** What question must this dashboard answer? Who's the audience?
* **Example placeholder:** 1 central question + a minimal "hook → tension → resolution" data story.
* **{bidux} angle:** `bid_interpret()` → nudge to add missing story bits/persona.
* **Analogy:** "Write the museum tour script before hanging the art."
* **Takeaway:** Focus narrows scope; scope simplifies design.

### 3) Structure: design for how people process

* **Goal:** Organize UI so the path to insight is obvious.
* **Example placeholder:** [Before/after] grouping, defaults, progressive disclosure.
* **{bidux} angle:** `bid_structure(layout=…)` → layout guidance + concept tips (e.g., proximity, defaults).
* **Analogy:** "Main exhibits first, side rooms later."
* **Takeaway:** Intentional structure beats feature piles.

### 4) Anticipate: design against bias/misread

* **Goal:** Prevent common interpretation errors.
* **Example placeholder:** Anchoring line; framing toggle (progress vs. gap).
* **{bidux} angle:** `bid_anticipate()` → surface likely biases + mitigation suggestions.
* **Analogy:** "Place signposts where visitors get lost."
* **Takeaway:** Small affordances avert big misinterpretations.

### 5) Validate: end with clarity & next steps

* **Goal:** Close the loop: "What does this mean? What should I do?"
* **Example placeholder:** Summary panel + 2–3 recommended actions.
* **{bidux} angle:** `bid_validate()` → prompts for summary/next steps/collab; optional report.
* **Analogy:** "A memorable final room + clear exit."
* **Takeaway:** The end shapes recall and adoption.

---

# Bring it together

* Final before/after
* Key takeaways
  1. Ground your design in behavioral science – it reduces user friction.
  2. Tools like reactable & echarts4r (and others in R) can implement these design changes readily.
  3. The {bidux} package is your “behavioral scientist in a pocket,” helping apply BID step by step.
  4. Ultimately: Build dashboards that are more useful and informative for your users
* Final punchline: "No more death-by-dropdown! By applying behavioral science through the BID framework – and with a little help from {bidux} – you can build Shiny apps that gently guide your users to insight and better decisions."

---

# Conclusion

* **Positioning:** *{bidux} is a "behavioral scientist in the console"*
  * lightweight prompts and checklists across all 5 stages.
* **What's new to mention (v0.2.0):** 
  * telemetry-driven notices
  * richer suggestions/prints
  * report generation (keep it to one line each)
* **Call to action:** "Try {bidux}; start with one stage on one app"

---

# Q&A slide

* Thank you!
* Links:CRAN/GitHub, minimal QR, contact.

