#lang dssl2

# HW1: DSSL2 Warmup

###
### ACCOUNTS
###

# an Account is either a checking or a saving account
let account_type? = OrC("checking", "savings")

class Account:
    let id
    let type
    let balance

    # Constructs an account with the given ID number, account type, and
    # balance. The balance cannot be negative.
    # Note: nat = natural number (0, 1, 2, ...)
    def __init__(self, id, type, balance):
        if balance < 0: error('Account: negative balance')
        if not account_type?(type): error('Account: unknown type')
        self.id = id
        self.type = type
        self.balance = balance

    # .get_balance() -> num?
    def get_balance(self): return self.balance

    # .get_id() -> nat?
    def get_id(self): return self.id

    # .get_type() -> account_type?
    def get_type(self): return self.type

    # .deposit(num?) -> NoneC
    # Deposits `amount` in the account. `amount` must be non-negative.
    def deposit(self, amount):
        if amount < 0:
            return error('Amount must be non-negative')
        self.balance = self.balance + amount

    # .withdraw(num?) -> NoneC
    # Withdraws `amount` from the account. `amount` must be non-negative
    # and must not exceed the balance.
    def withdraw(self, amount):
        if amount < 0 or amount > self.balance:
            return error('Incorrect Amount')
        self.balance = self.balance - amount

    # .transfer(num?, Account?) -> NoneC
    # Transfers the specified amount from this account to another. That is,
    # it subtracts `amount` from this account's balance and adds `amount`
    # to the `to` account's balance. `amount` must be non-negative.
    def transfer(self, amount, to):
        if self == to:
            self.balance = self.balance - amount
            to.deposit(amount)
        else:
            self.withdraw(amount)
            to.deposit(amount)
        
let account = Account(2, "checking", 32)        
test 'Account#transfer':
    let account = Account(2, "checking", 32)
    let account2 = Account(3, "checking", 42)
    account.transfer(10, account2) 
    assert account.get_balance() == 22
    assert account2.get_balance() == 52
    assert_error account.transfer(-10, account2)
    
test 'Account#transfer2':
    let account = Account(2, "checking", 32)
    let account2 = Account(3, "checking", 42)
    account.transfer(40, account)
    assert account.get_balance() == 32
    assert_error account.transfer(40, account2)
    
test 'Account#withdraw':
    let account = Account(2, "checking", 32)
    assert account.get_balance() == 32
    account.withdraw(10)
    assert account.get_balance() == 22
    assert_error account.withdraw(-10)
    assert_error account.withdraw(56)
    
test 'Account#deposit':
    let account = Account(2, "checking", 32)
    assert account.get_balance() == 32
    account.deposit(10)
    assert account.get_balance() == 42
    assert_error account.deposit(-10)


###
### CUSTOMERS
###

# Customers have names and bank accounts.
struct customer:
    let name
    let bank_account

# max_account_id(VecC[customer?]) -> nat?
# Find the largest account id used by any of the given customers' accounts.
# Raise an error if no customers are provided.
def max_account_id(customers):
    if customers == []:
        return error('No customers given')
    else:
        let max = customers[0].bank_account.get_id()
        for i in range(len(customers)):
            if customers[i].bank_account.get_id() > max:
                max = customers[i].bank_account.get_id()
        return max

let Aalaiah = customer("Aalaiah", Account(4,"savings",445))
let Khadijah = customer("Khadijah", Account(2,"checking",23))
let Amani = customer("Amani", Account(1,"checking",0))
let Andres = customer("Andres", Account(43,"savings",56))
let Joyce = customer("Joyce", Account(4,"checking",2243))

let customers = [Khadijah, Aalaiah]
let MTcustomers = []
let customers2 = [Amani, Andres, Joyce]
let customers3 = [Aalaiah, Amani, Andres, Joyce]
let c4 = [Joyce]

test 'Customers#max_account_id':
    assert max_account_id(customers2) == 43
    assert max_account_id(customers) == 4
    assert_error max_account_id(MTcustomers)

# open_account(str?, account_type?, VecC[customer?]) -> VecC[customer?]
# Produce a new vector of customers, with a new customer added. That new
# customer has the provided name, and their new account has the given type and
# a balance of 0. The id of the new account should be one more than the current
# maximum, or 1 for the first account created.
def open_account(name, type, customers):
    let New_customers = [0;len(customers) + 1]
    for i in range(len(customers)):
        New_customers.put(i, customers.get(i))
    if customers != []:
        let New_customer = customer(name, Account((max_account_id(customers) + 1), type, 0))
        let New_account = Account(New_customer.bank_account.get_id(), type, 0)
        New_customers.put(len(customers),New_customer)
    else:
        New_customers.put(0,customer(name, Account(1,type,0)))
    return New_customers

open_account("bob","savings",open_account("jim","checking",open_account("bill","savings",open_account("joe","checking",[])) )) 


test 'Customers#open_account':
    let Matt = customer("Matt", Account(5, "savings", 0))
    assert open_account("Matt", "savings", customers).get(2) == Matt
    assert open_account("Amani", "checking", MTcustomers).get(0) == Amani

# check_sharing(VecC[customer?]) -> bool?
# Checks whether any of the given customers share an account number.
def check_sharing(customers):
    for i in range(len(customers)):
        for j in range(1, len(customers)):
            if j != i and customers[i].bank_account.get_id() == customers[j].bank_account.get_id():  
                return True
    return False         
    
test 'Customers#check_sharing':
    assert check_sharing(customers) == False
    assert check_sharing(customers2) == False
    assert check_sharing(customers3) == True