#include <stdio.h>

typedef struct
{
    char *name;
    int price;
} Order;

int main()
{
    int pricesLen;
    scanf("%d\n", &pricesLen);
    Order prices[pricesLen];

    for (int i = 0; i < pricesLen; i++)
    {
        char *item;
        float price;
        scanf("%s %f\n", item, &price);
        prices[i] = (Order){item, price};
    }

    int ordersLen;
    scanf("%d\n", &ordersLen);
    char *orders[ordersLen];

    for (int i = 0; i < ordersLen; i++)
    {
        char *name;
        scanf("%s\n", name);

        int orderLen;
        scanf("%d\n", &orderLen);
        char *order[orderLen];
    }
}