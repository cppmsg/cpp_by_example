#include <future>
std::future<void>
process_login( std::string const& username,std::string const& password) {
    return spawn_async([=](){
        return backend.authenticate_user(username,password);
    }).then([](std::future<user_id> id){
        return backend.request_current_info(id.get());
    }).then([](std::future<user_data> info_to_display){
        try{
            update_display(info_to_display.get());
        } catch(std::exception& e){
            display_error(e);
        }
    });
}
