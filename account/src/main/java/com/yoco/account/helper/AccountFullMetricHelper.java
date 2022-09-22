package com.yoco.account.helper;

import com.fullmetrics.api.client.model.metrics.MetricsMap;
import com.yoco.commons.fullservices.FullMetrics;

public class AccountFullMetricHelper
{
    public static AccountFullMetricHelper getInstance(){
        return new AccountFullMetricHelper();
    }

    public static final String ACCOUNTS_DELETED = "accounts_deleted";
    public static final String ACCOUNTS_CREATED = "accounts_created";
    public static final String ACCOUNTS_CREATED_SOURCE = "account_src_";

    public void updateAccountDeletionMetric(String accountID){
        var metrics = new MetricsMap();
        metrics.incr(ACCOUNTS_DELETED,1);
        FullMetrics.pushMetricForAppLevel(accountID, metrics);
    }

    public void updateAccountCreationMetric(String accountID, String source){
        var metrics = new MetricsMap();
        metrics.incr(ACCOUNTS_CREATED,1);
        metrics.incr(ACCOUNTS_CREATED_SOURCE + source,1);
        FullMetrics.pushMetricForAppLevel(accountID, metrics);
    }
}
