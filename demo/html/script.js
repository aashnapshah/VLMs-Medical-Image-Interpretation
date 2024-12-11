const fileInput = document.getElementById('file-input');
const imagePreview = document.getElementById('image-preview');
const generateButton = document.getElementById('generate-button');
const outputTableBody = document.getElementById('output-table-body');
const useSampleButton = document.getElementById('use-sample-button');

let uploadedFile = null;

useSampleButton.addEventListener('click', () => {
    fileInput.click();
});

// Drag-and-Drop Events
imagePreview.addEventListener('dragover', (event) => {
    event.preventDefault();
    imagePreview.classList.add('dragging');
});

imagePreview.addEventListener('dragleave', () => {
    imagePreview.classList.remove('dragging');
});

imagePreview.addEventListener('drop', (event) => {
    event.preventDefault();
    imagePreview.classList.remove('dragging');

    const file = event.dataTransfer.files[0];
    if (file && file.type.startsWith('image/')) {
        uploadedFile = file;
        displayImage(file);
    } else {
        alert('Please upload a valid image file.');
    }
});

// Click to Upload
imagePreview.addEventListener('click', () => {
    fileInput.click();
});

fileInput.addEventListener('change', (event) => {
    const file = event.target.files[0];
    if (file && file.type.startsWith('image/')) {
        uploadedFile = file;
        displayImage(file);
    } else {
        alert('Please upload a valid image file.');
    }
});

// Display Image in Preview
function displayImage(file) {
    const reader = new FileReader();
    reader.onload = (e) => {
        imagePreview.innerHTML = `<img src="${e.target.result}" alt="Uploaded Image">`;
    };
    reader.readAsDataURL(file);
}

// Generate Prompt and Upload
generateButton.addEventListener('click', () => {
if (!uploadedFile) {
alert('Please upload an image first.');
return;
}

const model = document.getElementById('model').value;
const domain = document.getElementById('domain').value;
const evaluation = document.getElementById('evaluation').value;
const prompting = document.getElementById('prompting').value;

const formData = new FormData();
formData.append('image', uploadedFile);
formData.append('model', model);
formData.append('domain', domain);
formData.append('evaluation', evaluation);
formData.append('prompting', prompting);

fetch('/upload', {
method: 'POST',
body: formData
})
.then(response => response.json())
.then(data => {
    if (data.error) {
        alert(data.error);
        return;
    }

    // Check if the table has the placeholder row, and remove it
    const placeholderRow = document.querySelector('#output-table-body tr td[colspan="3"]');
    if (placeholderRow) {
        placeholderRow.parentElement.remove();
    }

    // Append a new row to the table
    const newRow = `
        <tr>
            <td>${data.model}</td>
            <td>${data.prompt}</td>
            <td>${data.response}</td>
            <td><a href="${data.image_url}" target="_blank">View Image</a></td>
        </tr>
    `;
    outputTableBody.innerHTML += newRow;
})
.catch(error => console.error('Error:', error));
});