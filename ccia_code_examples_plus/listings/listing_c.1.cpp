#include <mutex>
#include <condition_variable>
#include <queue>
#include <memory>
namespace Messaging {
    struct Message_base {
        virtual ~Message_base() {}
    };

    template<typename Msg>
    struct Wrapped_message: Message_base {
        Msg contents;
        explicit Wrapped_message(Msg const& contents_): contents(contents_) {}
    };
    class Queue {
        std::mutex m;
        std::condition_variable c;
        std::queue<std::shared_ptr<Message_base> > q;
    public:
        template<typename T>
        void push(T const& msg) {
            std::lock_guard<std::mutex> lk(m);
            q.push(std::make_shared<Wrapped_message<T> >(msg));
            c.notify_all();
        }
        std::shared_ptr<Message_base> wait_and_pop() {
            std::unique_lock<std::mutex> lk(m);
            c.wait(lk,[&]{return !q.empty();});
            auto res=q.front();
            q.pop();
            return res;
        }
    };
}
