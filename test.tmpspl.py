prices_len = int(input())
prices = []
for prices_idx in range(prices_len):
    (item, price) = input().split(" ")
    price = float(price)
    prices.append((item, price))

orders_len = int(input())
orders = []
for orders_idx in range(orders_len):
    name = input()
    order_len = int(input())
    order = []
    for order_idx in range(order_len):
        (quantity, item) = input().split(" ")
        quantity = int(quantity)
        order.append((quantity, item))

    orders.append((name, order))