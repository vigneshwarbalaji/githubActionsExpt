package com.yoco.commons.enums;

public enum PayrollStatus {

    DEFAULT_PAYROLL("DEFAULT_PAYROLL"),
    USER_ACKNOWLEDGED("USER-ACKNOWLEDGED"),
    ADMIN_UPDATED("ADMIN-UPDATED"),
    USER_CONFIRMED("USER-CONFIRMED"),
    ADMIN_APPROVED("ADMIN-APPROVED");

    private final String status;

    PayrollStatus(String status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return status;
    }
}
