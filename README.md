# NFT Lending Protocol

## Overview
The NFT Lending Protocol is a decentralized application that allows users to borrow funds by locking their NFTs as collateral. Users can create loans, repay them with interest, and retrieve loan details. This project includes a smart contract written in Clarity and a set of test cases to validate the contract's functionality.

---

## Features
- **Create Loan**: Users can create a loan by providing an NFT ID, the loan amount, and the loan duration.
- **Repay Loan**: Borrowers can repay their loans with interest to reclaim their NFTs.
- **Retrieve Loan Details**: Loans can be queried by their unique loan ID.
- **Interest Calculation**: Fixed interest rate of 10% for the MVP.

---

## Smart Contract Structure

### Constants
- `ERR-NOT-AUTHORIZED`: Error code for unauthorized access.
- `ERR-INVALID-AMOUNT`: Error code for invalid loan amounts.
- `ERR-LOAN-EXISTS`: Error code when a loan already exists.
- `ERR-NO-LOAN-FOUND`: Error code when no loan is found.

### Data Variables
- **`loans`**: A map storing loan details identified by `loan-id`.
- **`loan-nonce`**: A counter to generate unique loan IDs.

### Read-Only Functions
- `get-loan (loan-id uint)`: Retrieves loan details by loan ID.

### Public Functions
- `create-loan (nft-id uint) (amount uint) (duration uint)`:
  - Creates a new loan.
  - Locks the NFT and transfers the loan amount to the borrower.
  - Returns the unique loan ID.

- `repay-loan (loan-id uint)`:
  - Allows the borrower to repay the loan with interest.
  - Updates the loan status to "REPAID."

### Private Functions
- `calculate-interest-rate (nft-id uint)`:
  - Calculates a fixed interest rate of 10%.

- `calculate-repayment-amount (loan-id uint)`:
  - Computes the total repayment amount (loan + interest).

---

## Tests

The project includes comprehensive tests written in **Vitest** to ensure the smart contract operates as expected. These tests simulate blockchain interactions and verify the behavior of public functions.

### Test Cases
1. **Loan Creation**:
   - Valid loan creation.
   - Error handling for invalid loan amounts.

2. **Loan Repayment**:
   - Successful repayment by the borrower.
   - Unauthorized repayment attempts.
   - Repayment of non-existent loans.

3. **Loan Retrieval**:
   - Retrieve loan details by ID.
   - Handle queries for non-existent loans.

4. **Simnet Initialization**:
   - Ensures `simnet` is properly initialized for testing.

---

## File Structure

```plaintext
root
├── contracts
│   └── nftl.clar    # Clarity smart contract
├── tests
│   └── nftl.test.js # Vitest test cases
└── README.md                        # Project documentation
```

---

## How to Run

### Prerequisites
- Node.js and npm installed.
- Clarity development environment set up.
- Vitest for testing.

### Steps
1. **Clone the Repository**:
   ```bash
   git clone <repository-url>
   cd nft-lending-protocol
   ```

2. **Install Dependencies**:
   ```bash
   npm install
   ```

3. **Run Tests**:
   ```bash
   npm test
   ```

---

## Future Enhancements
- Dynamic interest rates based on market conditions.
- Support for multiple NFT standards.
- Partial loan repayments.
- Frontend interface for seamless user interactions.

---

## License
This project is open-sourced under the MIT License. See the LICENSE file for details.

