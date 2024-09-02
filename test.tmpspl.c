#include <stdio.h>

int main() {
    int prices_len;
    scanf("%d", &prices_len);
    for (int prices_idx = 0 ; prices_idx < prices_len ; prices_idx++) {
        char item[500];
        float price;
        scanf("%s %f", item,  &price);
    }

    int orders_len;
    scanf("%d", &orders_len);
    for (int orders_idx = 0 ; orders_idx < orders_len ; orders_idx++) {
        char order_name[500];
        gets(order_name);
        int order_len;
        scanf("%d", &order_len);
        for (int order_idx = 0 ; order_idx < order_len ; order_idx++) {
            int quantity;
            char item[500];
            scanf("%d %s", &quantity,  item);
        }

    }

}