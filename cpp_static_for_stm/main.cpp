#include <iostream>
using namespace std;

// TU/global/file Scope(s)/ or hidden_var!
extern int tu_extern_scope;
extern int tu_extern_init_scope{0};     // definition is not extern anymore
int tu_scope;                           // inited to 0
const int tu_const_scope = 0;
static int tu_static_scope;             // inited to 0

int g(int i){
    cout << "START g():" << endl;
    tu_extern_scope;
    static int qq_block_stm_scope_static{200};
    cout << "g:Hello World:" << ++qq_block_stm_scope_static << "," << i << endl;
    for ( static int h=2, k=4; h<10; ++h, ++tu_scope)  // TODO??: what is formal name for the scope of h?
    {                                   // block-statement scope
        int temp{};
        static int ff_block_stm_scope_static{300};
        cout << "g_for:Hello World:" << h <<","  << ++ff_block_stm_scope_static << "," << k << "," << temp << endl;
    }
    cout << "END g():" << endl;
    return 0;
}

int main()
{                                       // block scope
    for ( static int f=2, j=4; f<10; ++f, ++tu_scope)
    {                                   // block-statement scope
        int temp {10};
        cout << "Hello World:" << f <<"," << "," << j << "," << temp << endl;
        g(42);
    }

    cout << "###" << endl;
    return 0;
}
