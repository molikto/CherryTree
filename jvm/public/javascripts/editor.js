

var root = document.getElementById("editor")

new ClientInitializerView(root, uuid(root.getAttribute("data-document-id")), uuidOption(root.getAttribute("data-node-id")), true)
