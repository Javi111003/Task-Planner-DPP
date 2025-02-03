# Task-Planner-DPP

**A Declarative Task and Resource Planner for Workplace Optimization**  
*Third-Year Declarative Programming Project | Computer Science Degree*
- Javier Alejandro González Díaz
- Kevin Márquez Vega
- José Miguel Leyva de la Cruz
---

## 📜 Project Description

**Task-Planner-DPP** is a Haskell-based system designed to generate optimal task schedules for workplaces. It intelligently assigns tasks to workers while respecting constraints such as: 
- Worker availability and skills, 
- Resource/tool availability, 
- Task deadlines and priorities, 
- Workload balance across teams.

The planner uses **declarative programming paradigms** to model complex relationships between tasks, workers, and resources, enabling automatic exploration of valid assignments without imperative control flow.

---

## 🎯 Key Features

- **Automatic Task Prioritization**: Sorts tasks by deadlines and dependencies.
- **Resource Management**: Handles finite/infinite resources and tracks availability.
- **Skill-Based Assignments**: Ensures workers have required skills for tasks.
- **Time Slot Optimization**: Generates conflict-free schedules using `TimeSlot` constraints.
- **Workload Balancing**: Distributes tasks evenly across workers.
- **Extensible Rules**: Add new constraints (e.g., equipment sharing, team collaboration).

---

## 🧠 Declarative Programming Approach

This project leverages Haskell’s strengths in declarative programming:
- **Immutable Data**: `SystemState` safely models worker/resource availability.
- **Pure Functions**: Validation and scheduling logic is side-effect-free.
- **Expressive Types**: Rich type system for tasks (`Task`), workers (`Worker`), and resources (`Resource`).
- **Lazy Evaluation**: Efficiently explores possible schedules without redundant computations.

---

## 📦 Key Concepts

| Concept          | Description                                                                 |
|------------------|-----------------------------------------------------------------------------|
| **Task**         | A unit of work with skills, resources, duration, and deadline.             |
| **Worker**       | An employee with skills, availability days, and hourly limits.             |
| **Resource**     | A tool/equipment (e.g., `Exclusive` or `Infinite` type).                    |
| **TimeSlot**     | A time window (`Day`, start/end hour) for task assignments.                 |
| **SystemState**  | Global state tracking worker schedules, resource usage, and task assignments. |

---

## 🛠️ Installation & Usage

### Prerequisites
- [Haskell Stack](https://docs.haskellstack.org/)

### Steps
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/Task-Planner-DPP.git
   cd Task-Planner-DPP
