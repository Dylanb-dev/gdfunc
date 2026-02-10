// static/app.js

const sourceEditor = document.getElementById('sourceEditor');
const cCodeOutput = document.getElementById('cCodeOutput');
const scanOutput = document.getElementById('scanOutput');
const parseOutput = document.getElementById('parseOutput');
const typeCheckOutput = document.getElementById('typeCheckOutput');
const errorOutput = document.getElementById('errorOutput');
const errorMessage = document.getElementById('errorMessage');
const stdoutOutput = document.getElementById('stdoutOutput');
const stderrOutput = document.getElementById('stderrOutput');

const compileBtn = document.getElementById('compileBtn');
const runBtn = document.getElementById('runBtn');
const clearBtn = document.getElementById('clearBtn');
const exampleSelect = document.getElementById('exampleSelect');

// Compile to C
compileBtn.addEventListener('click', async () => {
    const source = sourceEditor.value;

    compileBtn.disabled = true;
    compileBtn.textContent = 'Compiling...';

    try {
        const response = await fetch('/api/compile', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ sourceCode: source }),
        });

        const result = await response.json();

        // Update compiler output
        scanOutput.textContent = result.scanOutput;
        parseOutput.textContent = result.parseOutput;
        typeCheckOutput.textContent = result.typeCheckOutput;

        if (result.success) {
            cCodeOutput.value = result.cCode;
            errorOutput.style.display = 'none';
            scanOutput.className = 'success';
            parseOutput.className = 'success';
            typeCheckOutput.className = 'success';
        } else {
            cCodeOutput.value = '// Compilation failed\n// See error below';
            errorOutput.style.display = 'block';
            errorMessage.textContent = result.errorMessage;
        }
    } catch (error) {
        errorOutput.style.display = 'block';
        errorMessage.textContent = 'Network error: ' + error.message;
    } finally {
        compileBtn.disabled = false;
        compileBtn.textContent = 'Compile to C';
    }
});

// Compile and Run C
runCBtn.addEventListener('click', async () => {
    const source = cCodeOutput.value;

    runCBtn.disabled = true;
    runCBtn.textContent = 'Running...';

    try {
        const response = await fetch('/api/runC', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ sourceCode: source }),
        });

        const result = await response.json();

        if (result.runSuccess) {
            stdoutOutput.textContent = result.runStdout || '(no output)';
            stderrOutput.textContent = result.runStderr || '(no errors)';
            errorOutput.style.display = 'none';
        } else {
            stdoutOutput.textContent = result.runStdout;
            stderrOutput.textContent = result.runStderr;
            errorOutput.style.display = 'block';
            errorMessage.textContent = result.runErrorMessage || 'Execution failed';
        }

    } catch (error) {
        errorOutput.style.display = 'block';
        errorMessage.textContent = 'Network error: ' + error.message;
    } finally {
        runCBtn.disabled = false;
        runCBtn.textContent = 'Compile and Run';
    }
});

// Compile and Run Both
runBtn.addEventListener('click', async () => {
    const source = sourceEditor.value;

    runBtn.disabled = true;
    runBtn.textContent = 'Running...';

    try {
        const response = await fetch('/api/run', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ sourceCode: source }),
        });

        const result = await response.json();

        if (result.runCompileSuccess) {
            cCodeOutput.value = result.runCCode;
            scanOutput.textContent = '✓ Scanned';
            parseOutput.textContent = '✓ Parsed';
            typeCheckOutput.textContent = '✓ Type checked';

            if (result.runSuccess) {
                stdoutOutput.textContent = result.runStdout || '(no output)';
                stderrOutput.textContent = result.runStderr || '(no errors)';
                errorOutput.style.display = 'none';
            } else {
                stdoutOutput.textContent = result.runStdout;
                stderrOutput.textContent = result.runStderr;
                errorOutput.style.display = 'block';
                errorMessage.textContent = result.runErrorMessage || 'Execution failed';
            }
        } else {
            errorOutput.style.display = 'block';
            errorMessage.textContent = result.runErrorMessage;
        }
    } catch (error) {
        errorOutput.style.display = 'block';
        errorMessage.textContent = 'Network error: ' + error.message;
    } finally {
        runBtn.disabled = false;
        runBtn.textContent = 'Compile and Run';
    }
});

// Clear all
clearBtn.addEventListener('click', () => {
    sourceEditor.value = '';
    cCodeOutput.value = '// C code will appear here...';
    scanOutput.textContent = '-';
    parseOutput.textContent = '-';
    typeCheckOutput.textContent = '-';
    stdoutOutput.textContent = '-';
    stderrOutput.textContent = '-';
    errorOutput.style.display = 'none';
});

// Load examples list on page load
async function loadExamplesList() {
    try {
        const response = await fetch('/api/examples');
        const examples = await response.json();

        // Populate dropdown
        examples.forEach(example => {
            const option = document.createElement('option');
            option.value = example.fileName;
            option.textContent = example.displayName;
            exampleSelect.appendChild(option);
        });
    } catch (error) {
        console.error('Failed to load examples:', error);
    }
}

// Load selected example
exampleSelect.addEventListener('change', async () => {
    const filename = exampleSelect.value;

    if (!filename) {
        return; // No selection
    }

    try {
        const response = await fetch(`/api/examples/${filename}`);
        const result = await response.json();

        if (result.exampleSuccess) {
            sourceEditor.value = result.exampleContent;
            // Clear outputs when loading new example
            cCodeOutput.value = '// C code will appear here...';
            scanOutput.textContent = '-';
            parseOutput.textContent = '-';
            typeCheckOutput.textContent = '-';
            stdoutOutput.textContent = '-';
            stderrOutput.textContent = '-';
            errorOutput.style.display = 'none';
        } else {
            errorOutput.style.display = 'block';
            errorMessage.textContent = result.exampleError || 'Failed to load example';
        }
    } catch (error) {
        errorOutput.style.display = 'block';
        errorMessage.textContent = 'Network error: ' + error.message;
    }
});

// Load examples on page load
loadExamplesList();