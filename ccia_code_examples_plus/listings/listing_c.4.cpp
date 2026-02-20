#include <memory>
namespace Messaging {

class Close_queue {};
class Dispatcher {
    Queue *q;
    bool   chained;
    Dispatcher(Dispatcher const &)            = delete;
    Dispatcher &operator=(Dispatcher const &) = delete;
    template <typename Dispatcher, typename Msg, typename Func> friend class TemplateDispatcher;

    void
    wait_and_dispatch() {
        for(;;) {
            auto msg = q->wait_and_pop();
            dispatch(msg);
        }
    }
    bool
    dispatch(std::shared_ptr<Message_base> const &msg) {
        if(dynamic_cast<Wrapped_message<Close_queue> *>(msg.get())) { throw Close_queue(); }
        return false;
    }

public:
    Dispatcher(Dispatcher &&other) : q(other.q), chained(other.chained) { other.chained = true; }

    explicit Dispatcher(Queue *q_) : q(q_), chained(false) {}

    template <typename Message, typename Func>
    TemplateDispatcher<Dispatcher, Message, Func>
    handle(Func &&f) {
        return TemplateDispatcher<Dispatcher, Message, Func>{ q, this, std::forward<Func>(f) };
    }

    ~Dispatcher() noexcept(false) {
        if(!chained) { wait_and_dispatch(); }
    }
    };

    }
