package com.yoco.project.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum PROJECT_CHANNEL_KEYS {

    PROJECT_KEY("project"),
    PROJECT_ASSOCIATION("PROJECT_ASSOCIATION"),
    PROJECT_DISASSOCIATION("PROJECT_DISASSOCIATION"),
    PROJECT_NAME_UPDATE("PROJECT_NAME_UPDATE"),
    YOCO_PROJECTS_ACTION("Projects_Operation");

    private String message;

    PROJECT_CHANNEL_KEYS  ( String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }

}
