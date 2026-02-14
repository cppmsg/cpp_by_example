#include <string>
#include <thread>
void open_document_and_display_gui(std::string const &filename) {}
bool done_editing() { return true; }
std::string get_filename_from_user() { return "foo.doc"; }

enum class Command_type :int { open_new_document };
struct User_command {
    Command_type type{};
    User_command() : type{Command_type::open_new_document} {}
};
User_command get_user_input() { return User_command{}; }
void process_user_input(User_command const &cmd) {}

void edit_document(std::string const &filename) {
    open_document_and_display_gui(filename);
    while( not done_editing() ) {
        User_command cmd{get_user_input()};
        if( cmd.type == Command_type::open_new_document ) {
            std::string const new_name{get_filename_from_user()};
            std::thread       t{edit_document, new_name};  // Recursive-like?
            t.detach();
        } else {
            process_user_input(cmd);
        }
    }
}

int main() {
    edit_document("bar.doc");
}
