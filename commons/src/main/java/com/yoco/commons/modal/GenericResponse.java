package com.yoco.commons.modal;

import com.yoco.commons.enums.error.ErrorCode;
import com.yoco.commons.utils.ObjUtils;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

@Data
@NoArgsConstructor
public class GenericResponse implements Serializable {

    private boolean success = false;
    private ErrorCode errorCode;
    private String errorMessage;
    private Map<String, Object> data;

    public GenericResponse(boolean success, ErrorCode errorCode, String errorMessage) {
        this.success = success;
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
    }

    public void add(String key, Object obj) {
        if (ObjUtils.isNull(data))
            data = new HashMap<>();
        data.put(key, obj);
    }

    public void addNonNull(String key, Object obj) {
        if (!ObjUtils.isNull(obj))
            add(key, obj);
    }
}
