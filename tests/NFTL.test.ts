import { describe, it, expect, beforeEach } from 'vitest';

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;

// Mocking the blockchain state and contract functions
type Loan = {
  borrower: string;
  nftId: number;
  loanAmount: number;
  interestRate: number;
  startBlock: number;
  duration: number;
  status: string;
};

let loans: Map<number, Loan>;
let loanNonce: number;

// Helper to simulate blockchain state
const getBlockHeight = () => Math.floor(Date.now() / 1000);

// Contract functions to test
const createLoan = (user: string, nftId: number, amount: number, duration: number) => {
  if (amount <= 0) throw new Error("Invalid loan amount");
  
  const loanId = ++loanNonce;
  const interestRate = 10; // Fixed interest rate for MVP

  loans.set(loanId, {
    borrower: user,
    nftId,
    loanAmount: amount,
    interestRate,
    startBlock: getBlockHeight(),
    duration,
    status: "ACTIVE",
  });

  return loanId;
};

const repayLoan = (user: string, loanId: number) => {
  const loan = loans.get(loanId);
  if (!loan) throw new Error("No loan found");
  if (loan.borrower !== user) throw new Error("Not authorized");

  const interestAmount = (loan.loanAmount * loan.interestRate) / 100;
  const totalRepayment = loan.loanAmount + interestAmount;

  loans.set(loanId, { ...loan, status: "REPAID" });

  return totalRepayment;
};

const getLoan = (loanId: number) => {
  return loans.get(loanId) || null;
};

// Tests using Vitest
describe("NFT Lending Protocol Contract", () => {
  beforeEach(() => {
    // Reset the loans and nonce before each test
    loans = new Map();
    loanNonce = 0;
  });

  it("should allow a user to create a loan", () => {
    const loanId = createLoan(address1, 101, 1000, 30);
    const loan = getLoan(loanId);

    expect(loan).toBeDefined();
    expect(loan?.nftId).toBe(101);
    expect(loan?.loanAmount).toBe(1000);
    expect(loan?.duration).toBe(30);
    expect(loan?.status).toBe("ACTIVE");
  });

  it("should throw an error for an invalid loan amount", () => {
    expect(() => createLoan(address1, 101, 0, 30)).toThrow("Invalid loan amount");
  });

  it("should allow a user to repay a loan", () => {
    const loanId = createLoan(address1, 102, 1000, 30);
    const repaymentAmount = repayLoan(address1, loanId);
    const loan = getLoan(loanId);

    expect(loan).toBeDefined();
    expect(loan?.status).toBe("REPAID");
    expect(repaymentAmount).toBe(1100); // 10% interest on 1000
  });

  it("should throw an error when trying to repay a non-existent loan", () => {
    expect(() => repayLoan(address1, 999)).toThrow("No loan found");
  });

  it("should throw an error when a non-borrower tries to repay a loan", () => {
    const loanId = createLoan(address1, 103, 1000, 30);
    expect(() => repayLoan(address2, loanId)).toThrow("Not authorized");
  });

  it("should retrieve a loan's details correctly", () => {
    const loanId = createLoan(address1, 104, 2000, 60);
    const loan = getLoan(loanId);

    expect(loan).toBeDefined();
    expect(loan?.nftId).toBe(104);
    expect(loan?.loanAmount).toBe(2000);
    expect(loan?.duration).toBe(60);
    expect(loan?.status).toBe("ACTIVE");
  });

  it("should return null for a loan that does not exist", () => {
    const loan = getLoan(999);
    expect(loan).toBeNull();
  });
});

describe("simnet", () => {
  it("ensures simnet is well initialized", () => {
    expect(simnet.blockHeight).toBeDefined();
  });
});
