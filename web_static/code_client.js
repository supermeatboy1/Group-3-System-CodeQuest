var ping_time = Date.now()
var old_files = null
var terminal_open = false
var terminal_line = ""

var files = [
	{
		"filename": "main.py",
		"content": "",
	},
];
var selected_filename = "main.py";

function addEvent( obj, evt, fn )
{
  if ( 'undefined' != typeof obj.addEventListener )
  {
    obj.addEventListener( evt, fn, false );
  }
  else if ( 'undefined' != typeof obj.attachEvent )
  {
    obj.attachEvent( "on" + evt, fn );
  }
}

async function sleep(delay) {
	return new Promise(resolve => setTimeout(resolve, delay))
}

function update_local_content() {
	var code_tabs = document.getElementById("code_tabs")
	while (code_tabs.lastElementChild) {
		code_tabs.removeChild(code_tabs.lastElementChild)
	}
	for (var i = 0; i < files.length; i++) {
		var file = files[i];

		var tab = document.createElement("div")
		if (i == 0) {
			tab.setAttribute("aria-selected", true)
		} else {
			tab.setAttribute("aria-selected", false)
		}
		tab.classList.add("tab")
		tab.id = "code_tab_" + i;

		var tab_text = document.createElement("span")
		tab_text.innerHTML = file["filename"]
		tab_text.classList.add("tab_text")

		tab.setAttribute("onmouseenter", "tab_close_button_set_visibility(this, \"" + file["filename"] + "\", true)")
		tab.setAttribute("onmouseleave", "tab_close_button_set_visibility(this, \"" + file["filename"] + "\", false)")
		tab_text.setAttribute("onclick", "file_switch(\"" + file["filename"] + "\")")

		tab.appendChild(tab_text)
		code_tabs.appendChild(tab)
	}
	document.getElementById("code_input").value = files[0]["content"];
	selected_filename = files[0]["filename"];
}

function update_source_textarea_content() {
	for (var i = 0; i < files.length; i++) {
		if (files[i]["filename"] === selected_filename) {
			document.getElementById("code_input").value = files[i]["content"]
			break
		}
	}
}

function file_save(filename) {
	for (var i = 0; i < files.length; i++) {
		if (files[i]["filename"] === selected_filename) {
			files[i]["content"] = document.getElementById("code_input").value
			break
		}
	}
}

function file_switch(filename) {
	file_save(selected_filename)
	file_select(filename)
}

function file_select(filename) {
	var children = document.getElementById("code_tabs").children
	var element = null;

	for (var i = 0; i < children.length; i++) {
		children[i].setAttribute("aria-selected", "false")

		var tab_children = children[i].children;
		for (var j = 0; j < tab_children.length; j++) {
			var tc = tab_children[j];
			if (tc.tagName.toLowerCase() === "span" && tc.innerHTML === filename) {
				element = children[i];
				break;
			}
		}
	}

	if (element != null) {
		element.setAttribute("aria-selected", "true")
	}

	selected_filename = filename
	update_source_textarea_content();

	send_code_update();
}

function file_delete(element, filename) {
	if (files.length <= 1) {
		return
	}

	/* If we are deleting the current file,
	 * switch the current selected source file to
	 * another file that is adjacent to the deleted file.
	 *
	 */
	var new_index = -1;
	if (selected_filename === filename) {
		for (var i = 0; i < files.length; i++) {
			if (files[i]["filename"] === filename) {
				new_index = i - 1;
				if (new_index < 0) {
					new_index = 0;
				}
				break;
			}
		}
	}

	var code_tabs = document.getElementById("code_tabs")
	code_tabs.removeChild(element.parentNode)

	var new_files = files.filter(function (file) {
		return file["filename"] !== filename
	});

	files = new_files

	if (new_index >= 0) {
		selected_filename = files[new_index]["filename"]
		console.log("new filename: " + selected_filename)
		file_select(files[new_index]["filename"])
	} else {
		update_source_textarea_content()
	}
}

function tab_close_button_set_visibility(element, filename, button_visible) {
	if (files.length <= 1) {
		return
	}

	if (button_visible) {
		var button = document.createElement("button")
		button.innerHTML = "X"
		button.classList.add("tab_close")
		button.setAttribute("type", "button")
		button.setAttribute("onclick", "file_delete(this, \"" + filename + "\")")
		element.appendChild(button);
	} else {
		var children = element.children;
		for (var i = 0; i < children.length; i++) {
			if (children[i].tagName.toLowerCase() == "button") {
				element.removeChild(children[i])
			}
		}
	}
}

async function receive_source_files() {
	ws.send(JSON.stringify({
		"start_coding": "python",
	}));

	var received_data = JSON.parse(await ws.receive());
	var data_arr = received_data["files"];
	files = data_arr;

	update_local_content();
}

ws = new WebSocket("ws://127.0.0.1:6588/")

function heartbeat() {
	if (ws && ws.readyState === 1) {
		ws.send("{\"heartbeat\": 1}")
		ping_time = Date.now()
	}
	setTimeout(heartbeat, 10000)
}

ws.receive = () => {
	return new Promise(function(resolve, reject) {
		ws.onmessage = (message) => {
			if (message.data.trim() === "heartbeat_received") {
				var ping_ms = Date.now() - ping_time
				if (ping_ms < 0) {
					ping_ms = 0
				}
				var ping_p = document.getElementById("ping_ms")
				ping_p.innerHTML = "Ping: " + ping_ms + " ms"
			} else if (terminal_open) {
				try {
					var obj = JSON.parse(message.data);

					if (obj.hasOwnProperty("console_update")) {
				    var update = obj.console_update;

				    if (update.stdout) {
				        append_terminal(update.stdout);
				    } else if (update.stderr) {
				        append_terminal(update.stderr);
				    }

				    else if (update.exit) {
				        if (update.exit === "normal") {
				            append_terminal("Process terminated successfully.");
				        } else {
				            const exitStatus = update.exit.exit_status;
				            const exitReason = exitStatus !== undefined
				                ? `Process terminated with the following exit code: ${exitStatus}`
				                : `Process terminated with the following reason: ${update.exit}`;
				            append_terminal(exitReason);
				        }
				    }
					}
				} catch (e) {}
				console.log("Received message: ", message.data)
			}
			return resolve(message.data);
		}

		ws.onerror = (error) => {
			return reject(error);
		}
   });
}

ws.onopen = () => {
	console.log("Connection opened")
	receive_source_files();
}
ws.onclose = (event) => {
	console.log("Connection closed: ", event.code, event.reason)
}
ws.onerror = () => {
	console.log("Connection closed due to error.")
}

function send_code_update() {
	file_save(selected_filename)

	if (old_files == null) {
		console.log("This is the first ever code submission!")

		var update_list = []

		for (var i = 0; i < files.length; i++) {
			update_list.push({
				"filename": files[i]["filename"],
				"action": {
					"create": {
						"data": files[i]["content"]
					}
				},
			})
		}
		ws.send(JSON.stringify({
			"file_update": update_list
		}));
	} else {
		var update_list = []

		for (var i = 0; i < old_files.length; i++) {
			var old_file = old_files[i]
			var file = null
			for (var j = 0; j < files.length; j++) {
				if (files[j]["filename"] === old_file["filename"]) {
					file = files[j]
				}
			}
			if (file != null) {
				if (file["content"] === old_file["content"]) {
					// No update
				} else {
					// Calculate the difference between the two list of files.
					var old_lines = difflib.stringAsLines(old_file["content"])
					var new_lines = difflib.stringAsLines(file["content"])
					var seq_match = new difflib.SequenceMatcher(old_lines, new_lines)
					var opcodes = seq_match.get_opcodes()
					var operations_list = []
					
					for (var k = 0; k < opcodes.length; k++) {
						var opcode = opcodes[k]
						var tag = opcode[0]
						var i1 = opcode[1]
						var i2 = opcode[2]
						var j1 = opcode[3]
						var j2 = opcode[4]
						if (tag === "equal") {
							operations_list.push({
								"equal": {
									"start": i1,
									"end": i2
								}
							})
						} else if (tag === "replace" || tag === "insert") {
							var append_str = new_lines.slice(j1, j2)
							operations_list.push({
								"append": append_str,
							})
						}
					}

					update_list.push({
						"filename": old_file["filename"],
						"action": {
							"update": operations_list
						}
					})
				}
			} else {
				update_list.push({
					"filename": old_file["filename"],
					"action": "delete"
				})
			}
		}

		ws.send(JSON.stringify({
			"file_update": update_list
		}));
	}
	old_files = JSON.parse(JSON.stringify(files))
}

async function try_execute() {
	if (is_terminal_visible()) {
		return
	}

	send_code_update();

	console.log("Sending execution attempt to server...");
	ws.send(JSON.stringify({
		"try_execute": 1
	}))

	await ws.receive()

	var value = await ws.receive()
	console.log(value)
	var received_data = JSON.parse(value)
	if ("exec_start" in received_data) {
		clear_terminal()
		open_terminal()
	}
}

function close_terminal() {
	terminal = document.getElementById("terminal")
	terminal.style.display = "none";

	ws.send(JSON.stringify({
		"close_terminal": 1
	}))

	terminal_open = false
}

function open_terminal() {
	terminal = document.getElementById("terminal")
	terminal.style.display = "block";
	terminal_open = true;
}

function clear_terminal() {
	terminal_out = document.getElementById("terminal_out")
	terminal_out.value = "";
}

function is_terminal_visible() {
	return document.getElementById("terminal").style.display === "block"
}

function append_terminal(append_str) {
	if (!is_terminal_visible()) {
		return;
	}
	terminal_out = document.getElementById("terminal_out")
	terminal_out.value += append_str;
}

function backspace_terminal(append_str) {
	if (!is_terminal_visible()) {
		return;
	}
	terminal_out = document.getElementById("terminal_out")
	if (terminal_out.value.length > 0) {
		terminal_out.value = terminal_out.value.slice(0, -1);
	}
}

function capture_terminal_keypress(event) {
	if (!is_terminal_visible()) {
		return;
	}
	if (event.keyCode == 13 /* Enter */) {
		ws.send(JSON.stringify({
			"terminal_input": terminal_line
		}))
		append_terminal("\r\n")
		terminal_line = ""
	} else if (event.key.length === 1) {
		terminal_line += event.key
		append_terminal(event.key)
	}
}

function capture_terminal_keyup(event) {
	if (!is_terminal_visible()) {
		return;
	}
	if (event.keyCode == 8 /* Backspace */ && terminal_line.length > 0) {
		terminal_line = terminal_line.slice(0, -1)
		backspace_terminal()
	}
}

function client_initialize() {
	heartbeat()
	update_local_content()
	clear_terminal()

	document.getElementById("terminal_out").onkeypress = capture_terminal_keypress
	document.getElementById("terminal_out").onkeyup = capture_terminal_keyup

	document.getElementById('code_input').addEventListener('keydown', function(e) {
		if (e.key == 'Tab') {
			e.preventDefault()
			var start = this.selectionStart
			var end = this.selectionEnd

			this.value = this.value.substring(0, start) +
				"\t" + this.value.substring(end)

			this.selectionStart =
				this.selectionEnd = start + 1
		}
	})
}

function try_test_cases() {

}

document.onload = client_initialize()
