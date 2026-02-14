#include <string>
struct Card_inserted { std::string account;  };

int my_int{};
typedef int Special_int_alias_AKA_typedef_AKA_using_thingy;
using Special_int_alias_AKA_typedef_AKA_using_thingy2 = int;

int simple_f_fn_signature_XXX(std::string) {return 42;}
typedef int( * simple_f_fn_signature_XXX_alias) ( std::string );  // allias of a type or "to a type"? // inside out, right to left


//typedef int (*)(std::string) simple_f_fn_signature_old;
//using simple_f_fn_signature_new= int (*)(std::string);

class Atm {
    Messaging::receiver incoming;
    Messaging::sender   bank;
    Messaging::sender   interface_hardware;
    std::string account{};      // data member
    std::string pin{};
    // TODO??: std:: function?? void (Atm::*state)();  // data member (a ptr to something):      "pointer to member function" TODO??: what is this?


    void (Atm::*state)();  // data member (a ptr to something, a fn /c this param and returns void: "pointer to member function" TODO??: what is this?
    void waiting_for_card() {
        interface_hardware.send(display_enter_card());
        incoming.wait()
            .handle<Card_inserted>(
                [&](Card_inserted const& msg)
                {
                    account=msg.account;
                    pin="";
                    interface_hardware.send(display_enter_pin());
                    state=&Atm::getting_pin;
                }
                );
    }
    void getting_pin();
public:
    void run() {
        state=&Atm::waiting_for_card;
        try { for(;;) { (this->*state)(); } }
        catch(Messaging::close_queue const&) { }
    }
};
