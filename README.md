# 🎓 EduLoan Tracker

A smart contract for managing student loan repayments on the Stacks blockchain. Track loans, payments, and manage educational debt with transparency and automation.

## 📋 Features

- 🏦 **Loan Creation**: Lenders can create student loans with custom terms
- 💰 **Payment Tracking**: Borrowers can make payments and track progress
- 📊 **Loan Status**: Real-time loan status and payment history
- ⚠️ **Default Management**: Lenders can mark loans as defaulted
- 🔧 **Term Updates**: Flexible loan term modifications
- 📈 **Interest Calculation**: Automatic interest calculations
- ⏰ **Overdue Detection**: Track overdue payments automatically

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Clarity smart contracts

### Installation

```bash
git clone <your-repo>
cd eduloan-tracker
clarinet check
```

## 📖 Usage

### Creating a Loan

Lenders can create new student loans:

```clarity
(contract-call? .Eduloan create-loan 
  'ST1BORROWER-ADDRESS
  u50000    ;; Principal amount (50,000 STX)
  u5        ;; Interest rate (5%)
  u48       ;; Term in months (4 years)
  u1200)    ;; Monthly payment (1,200 STX)
```

### Making Payments

Borrowers can make loan payments:

```clarity
(contract-call? .Eduloan make-payment u1 u1200)
```

### Checking Loan Status

Anyone can view loan information:

```clarity
(contract-call? .Eduloan get-loan-status u1)
```

### Viewing Payment History

Check specific payment details:

```clarity
(contract-call? .Eduloan get-payment-history u1 u5)
```

## 🔍 Read-Only Functions

- `get-loan(loan-id)` - Get complete loan details
- `get-borrower-loans(borrower)` - Get all loans for a borrower
- `get-lender-loans(lender)` - Get all loans for a lender
- `get-loan-status(loan-id)` - Get current loan status
- `calculate-total-interest(loan-id)` - Calculate total interest
- `is-payment-overdue(loan-id)` - Check if payment is overdue
- `get-total-loans()` - Get total number of loans

## 🏗️ Public Functions

- `create-loan()` - Create a new student loan
- `make-payment()` - Make a loan payment
- `mark-loan-default()` - Mark loan as defaulted (lender only)
- `update-loan-terms()` - Update loan terms (lender only)

## 🛡️ Error Codes

- `u100` - Unauthorized access
- `u101` - Loan not found
- `u102` - Loan already exists
- `u103` - Invalid amount
- `u104` - Loan fully paid
- `u105` - Insufficient payment
- `u106` - Loan not active

## 🧪 Testing

Run the test suite:

```bash
clarinet test
```

## 📝 Contract Architecture

The contract uses several data structures:

- **loans**: Main loan data storage
- **borrower-loans**: Maps borrowers to their loan IDs
- **lender-loans**: Maps lenders to their loan IDs  
- **payment-history**: Tracks all payment transactions

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

## 🆘 Support

For questions and support, please open an issue in the GitHub repository.

---

Built with ❤️ for students and educational institutions on Stacks blockchain.

