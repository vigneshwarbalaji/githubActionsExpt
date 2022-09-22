package com.yoco.project.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum PROJECT_ACTIVITY {

    OPERATIONS("Project_Operations"),
    ENABLED("Project_Enabled");

    private String message;

    PROJECT_ACTIVITY  ( String message ) {
        this.message = message;
    }

    public String value ()
    {
        return message;
    }
}
