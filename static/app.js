async function loadTasks() {
    try {
        const response = await fetch("http://localhost:3000/api/tasks");
        if (!response.ok) throw new Error("Error al obtener tareas");

        const tasks = await response.json();
        const taskContainer = document.getElementById("tasks");
        const taskListHeader = document.getElementById("taskListHeader");
        taskContainer.innerHTML = "";

        if (tasks.length === 0) {
            taskContainer.innerHTML = "<p>No hay tareas disponibles.</p>";
            taskListHeader.style.display = "none";
            return;
        }

        taskListHeader.style.display = "block";
        tasks.forEach(task => {
            let taskElement = document.createElement("div");
            taskElement.className = "task";
            taskElement.innerHTML = `
                <strong>📝 ${task.taskId}</strong><br>
                <span>Descripción: ${task.description}</span><br>
                <span>Duración: ${task.estimatedTime} horas</span><br>
                <span>Inicio: ${task.startDate}</span><br>
                <span>Deadline: ${task.deadline}</span><br>
                <span>Prioridad: ${task.priority}</span><br>
                <span>Skills: ${task.requiredSkills.join(", ") || "Ninguno"}</span>
                <button class="modern-btn" onclick="deleteTask(${task.taskId})">❌</button>
            `;
            taskContainer.appendChild(taskElement);
        });

    } catch (error) {
        console.error("Error cargando tareas:", error);
        document.getElementById("tasks").innerHTML = "<p style='color: red;'>Error al cargar las tareas.</p>";
    }
}

async function deleteTask(taskId) {
    try {
        const response = await fetch(`http://localhost:3000/api/tasks/${taskId}`, {
            method: "DELETE"
        });

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(`Error al eliminar tarea: ${errorText}`);
        }

        console.log("Tarea eliminada:", taskId);
        loadTasks();
    } catch (error) {
        console.error("Error eliminando tarea:", error);
        alert("Error al eliminar la tarea. Consulte la consola para más detalles.");
    }
}

async function addTask() {
    try {
        const description = document.getElementById("taskName").value.trim();
        const estimatedTime = parseInt(document.getElementById("taskDuration").value, 10);
        const startDate = new Date().toISOString().split("T")[0]; 
        const deadline = document.getElementById("taskDeadline").value;
        const priority = "High";  
        const requiredSkills = document.getElementById("taskSkills").value
            .split(",")
            .map(s => s.trim())
            .filter(s => s.length > 0);

        if (!description || isNaN(estimatedTime) || !deadline) {
            alert("Por favor, complete todos los campos.");
            return;
        }

        const newTask = {
            taskId: 0,
            description,
            estimatedTime,
            startDate,
            deadline,
            priority,
            requiredSkills,
            requiredResources: [],
            dependencies: [],
            toDoSlots: []
        };

        console.log("Datos de la nueva tarea:", newTask);

        const response = await fetch("http://localhost:3000/api/tasks", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(newTask)
        });

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(`Error al agregar tarea: ${errorText}`);
        }

        console.log("Respuesta del servidor:", await response.json());

        loadTasks();
    } catch (error) {
        console.error("Error añadiendo tarea:", error);
        alert("Error al agregar la tarea. Consulte la consola para más detalles.");
    }
}

async function loadWorkers() {
    try {
        const response = await fetch("http://localhost:3000/api/workers");
        if (!response.ok) throw new Error("Error al obtener trabajadores");

        const workers = await response.json();
        const workerContainer = document.getElementById("workers");
        const workerListHeader = document.getElementById("workerListHeader");
        workerContainer.innerHTML = "";

        if (workers.length === 0) {
            workerContainer.innerHTML = "<p>No hay trabajadores disponibles.</p>";
            workerListHeader.style.display = "none";
            return;
        }

        workerListHeader.style.display = "block";
        workers.forEach(worker => {
            let workerElement = document.createElement("div");
            workerElement.className = "worker";
            workerElement.innerHTML = `
                <strong>👤 ${worker.workerId} - ${worker.workerName}</strong><br>
                <span>Skills: ${worker.skills.join(", ") || "Ninguno"}</span><br>
                <button class="modern-btn" onclick="deleteWorker(${worker.workerId})">❌</button>
            `;
            workerContainer.appendChild(workerElement);
        });

    } catch (error) {
        console.error("Error cargando trabajadores:", error);
        document.getElementById("workers").innerHTML = "<p style='color: red;'>Error al cargar los trabajadores.</p>";
    }
}

async function deleteWorker(workerId) {
    try {
        const response = await fetch(`http://localhost:3000/api/workers/${workerId}`, {
            method: "DELETE"
        });

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(`Error al eliminar trabajador: ${errorText}`);
        }

        console.log("Trabajador eliminado:", workerId);
        loadWorkers();
    } catch (error) {
        console.error("Error eliminando trabajador:", error);
        alert("Error al eliminar el trabajador. Consulte la consola para más detalles.");
    }
}

async function addWorker() {
    try {
        const name = document.getElementById("workerName").value.trim();
        const skills = document.getElementById("workerSkills").value
            .split(",")
            .map(s => s.trim())
            .filter(s => s.length > 0);

        if (!name) {
            alert("Por favor, complete todos los campos.");
            return;
        }

        const newWorker = {
            workerId: 0,
            workerName: name,
            availableDays: [],
            maxHoursPerDay: 8,
            skills ,
            currentSchedule: []
        };

        console.log("Datos del nuevo trabajador:", newWorker);

        const response = await fetch("http://localhost:3000/api/workers", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(newWorker)
        });

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(`Error al agregar trabajador: ${errorText}`);
        }

        console.log("Respuesta del servidor:", await response.json());

        loadWorkers();
    } catch (error) {
        console.error("Error añadiendo trabajador:", error);
        alert("Error al agregar el trabajador. Consulte la consola para más detalles.");
    }
}
async function calculateAssignment() {
    try {
        const response = await fetch("http://localhost:3000/api/schedule", {
            method: "POST"
        });
        if (!response.ok) throw new Error("Error al calcular la asignación de tareas");

        const [assignedTasks, unassignedTasks] = await response.json();
        const assignmentContainer = document.getElementById("assignment");
        const unassignedTasksContainer = document.getElementById("unassignedTasks");
        const assignmentListHeader = document.getElementById("assignmentListHeader");
        const unassignedTaskListHeader = document.getElementById("unassignedTaskListHeader");
        assignmentContainer.innerHTML = "";
        unassignedTasksContainer.innerHTML = "";

        if (assignedTasks.length === 0) {
            assignmentContainer.innerHTML = "<p>No se pudo calcular la asignación de tareas.</p>";
            assignmentListHeader.style.display = "none";
        } else {
            assignmentListHeader.style.display = "block";
            assignedTasks.forEach(([task, workers, timeSlot]) => {
                let assignmentElement = document.createElement("div");
                assignmentElement.className = "assignment";
                assignmentElement.innerHTML = `
                    <strong>📝 Tarea: ${task.taskId} - ${task.description}</strong><br>
                    <span>Trabajadores asig. : ${workers.map(w => w.workerName).join(", ")}</span><br>
                    <span>Horario de realiz. : ${timeSlot.startHour}:00 - ${timeSlot.endHour}:00</span>
                `;
                assignmentContainer.appendChild(assignmentElement);
            });
        }

        if (unassignedTasks.length === 0) {
            unassignedTasksContainer.innerHTML = "<p>Todas las tareas fueron asignadas.</p>";
            unassignedTaskListHeader.style.display = "none";
        } else {
            unassignedTaskListHeader.style.display = "block";
            unassignedTasks.forEach(task => {
                let unassignedTaskElement = document.createElement("div");
                unassignedTaskElement.className = "unassigned-task";
                unassignedTaskElement.innerHTML = `
                    <strong>📝 Tarea: ${task.taskId} - ${task.description}</strong><br>
                    <span>Duración: ${task.estimatedTime} horas</span><br>
                    <span>Deadline: ${task.deadline}</span><br>
                    <span>Prioridad: ${task.priority}</span><br>
                    <span>Skills: ${task.requiredSkills.join(", ") || "Ninguno"}</span>
                `;
                unassignedTasksContainer.appendChild(unassignedTaskElement);
            });
        }

    } catch (error) {
        console.error("Error calculando la asignación de tareas:", error);
        document.getElementById("assignment").innerHTML = "<p style='color: red;'>Error al calcular la asignación de tareas.</p>";
        document.getElementById("unassignedTasks").innerHTML = "<p style='color: red;'>Error al calcular las tareas no asignadas.</p>";
    }
}