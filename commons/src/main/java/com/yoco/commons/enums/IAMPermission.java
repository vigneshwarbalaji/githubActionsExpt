package com.yoco.commons.enums;

public enum IAMPermission {

    CLOCK("yoco-api.account.clock"),

    REMINDER("yoco-api.account.clock.reminder"),

    ACTIVITY("yoco-api.account.activity"),

    WEEKLY_DIGEST("yoco-api.account.weekly-digest"),

    FORCE_CLOCK_OUT("yoco-api.account.force_clockout"),

    REPORTS_EDIT("yoco-api.account.reports.edit"),

    REPORTS_EDIT_ALL("yoco-api.account.reports.edit-all"),

    REPORTS_VIEW("yoco-api.account.reports.view"),

    REPORTS_VIEW_ALL("yoco-api.account.reports.view-all"),

    ADJUSTMENTS_EDIT("yoco-api.account.adjustments.edit"),

    ADJUSTMENTS_VIEW("yoco-api.account.adjustments.view"),

    CONFIRM_HOURS("yoco-api.account.payroll.confirm"),

    ADMIN("yoco-api.account.admin"),

    SUPER_ADMIN("yoco-api.account.super.admin");

    private final String permission;

    IAMPermission(String permission) {
        this.permission = permission;
    }

    @Override
    public String toString() {
        return permission;
    }
}
