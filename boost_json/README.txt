Readme for directory of: grostig_tools
Copyright 2025 Grant Rostig
License: Boost v1.0

Headers, Source files, and others usefull to Modern C++ development projects

*** Searchable Symbols in the Source Code ***
This code is open to code-review, please note that:
SYMBOL     MEANING
 TODO:	   the principal programmer needs to: add, or do, or fix by him/herself.
 TODO?:	   the principal programmer is not sure about something, that should be addressed at some point.
 TODO?X:   the X/other programmer is not sure about something, that should be addressed at some point.
 TODO??:   is a question for verbal discussion at CppMSG.com meetup meetings where the programmer would like help from others.
 +++++++++++++++++++++++++++++++++++++++++++
Create, code and show a JSON file used for prompting LLM to produce c++ program code and files.
Create a JSON file that defines a structured prompt for an LLM to generate C++ program code and files. This will include specifications for the program requirements, code structure, and file organization.
+++++++++++++++++++++++++++++++++++++++++++
 How to use this JSON

     Feed the entire JSON to your LLM of choice.

     Instruct the model: “Read the prompt section and output each file under files as separate files with the given paths and content templates filled in.”

     The LLM will then generate a real C++ codebase matching your requirements and structure—perfect for scaffolding new projects.
+++++++++++++++++++++++++++++++++++++++++++
Below is an example of a structured JSON “prompt schema” that you can feed into an LLM to have it generate a complete C++ project—including requirements, directory layout, and individual files with boilerplate. Feel free to adapt field names or add extra metadata as needed.
+++++++++++++++++++++++++++++++++++++++++++

