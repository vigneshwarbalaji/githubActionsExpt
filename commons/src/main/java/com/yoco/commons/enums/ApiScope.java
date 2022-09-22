package com.yoco.commons.enums;

import java.util.HashSet;
import java.util.Set;

public enum ApiScope {

    AWAPIS_ACCOUNT_CREATE("awapis.account.create"),

    AWAPIS_CLOCK_SYNC("awapis.clock.sync"),

    AWAPIS_FULLACCESS("awapis.fullaccess"),

    AWAPIS_CHANNEL_PUBLISH("awapis.message.publish"),

    YOCOAPIS_ACC_READ("yoco-api.account.read"),

    YOCOAPIS_ACC_READ_WRITE("yoco-api.account.read_write"),

    YOCOAPIS_CLIENT_READ("yoco-api.account.client.read"),

    YOCOAPIS_CLIENT_READ_WRITE("yoco-api.account.client.read_write"),

    YOCOAPIS_CLOCK_READ("yoco-api.account.clock.read"),

    YOCOAPIS_CLOCK_READ_WRITE("yoco-api.account.clock.read_write"),

    YOCOAPIS_REPORTS_READ("yoco-api.account.reports.read"),

    YOCOAPIS_REPORTS_READ_WRITE("yoco-api.account.reports.read_write"),

    YOCOAPIS_FULLACCESS("yoco-api.fullaccess"),

    YOCOAPIS_IDENTITY("yoco-api.identity"),

    YOCOAPIS_PROJECT_READ("yoco-api.account.project.read"),

    YOCOAPIS_PROJECT_READ_WRITE("yoco-api.account.project.read_write"),

    YOCOAPIS_FULL_ACCESS("yoco-api.fullaccess"),

    CONTACTAPIS_FULL_ACCESS("contacts-api.full_access");

    final String scope;

    ApiScope(String scope) {
        this.scope = scope;
    }

    @Override
    public String toString() {
        return scope;
    }

    public static Set<String> getServiceTokenScopes(){
        Set<String> scopes  =  new HashSet<>();
        scopes.add(ApiScope.YOCOAPIS_FULL_ACCESS.toString());
        scopes.add(ApiScope.CONTACTAPIS_FULL_ACCESS.toString());
        scopes.add(ApiScope.AWAPIS_CLOCK_SYNC.toString());
        scopes.add(ApiScope.AWAPIS_CHANNEL_PUBLISH.toString());
        scopes.add(ApiScope.AWAPIS_ACCOUNT_CREATE.toString());
        return scopes;
    }

    public static Set<String> getAwAccountCreationServiceTokenScopes(){
        Set<String> scopes  =  new HashSet<>();
        scopes.add(ApiScope.AWAPIS_ACCOUNT_CREATE.toString());
        return scopes;
    }

    public static Set<String> getAccessTokenScopes(){
        Set<String> scopes  =  new HashSet<>();
        scopes.add(ApiScope.YOCOAPIS_FULL_ACCESS.toString());
        scopes.add(ApiScope.CONTACTAPIS_FULL_ACCESS.toString());
        return scopes;
    }


}