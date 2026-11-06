#include <stdint.h>
volatile uint32_t* address = (volatile uint32_t*)0xFFFFFF0C;
int main(){
    int array[1000];
    for(int i=0;i<1000;i++){
        array[i]=i+1;
    }
    int sum=0;//500500
    for(int i=0;i<1000;i++){
        sum+=array[i];
    }
    *address=sum;
    return 0;
}