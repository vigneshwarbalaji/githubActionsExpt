package com.yoco.commons.utils;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class ObjUtils {

    private ObjUtils(){}

    public static boolean isNullOrEmpty(String value) {
        return (value == null || value.trim().length() < 1);
    }

    public static boolean isNullOrEmpty(Object[] array) {
        return (array == null || array.length < 1);
    }

    public static boolean isNull(Object obj) {
        return obj == null;
    }

    public static boolean isNullOrEmpty(Collection<?> obj) {
        return (obj == null || obj.isEmpty());
    }

    public static boolean isNullOrEmpty(Map<?, ?> map) {
        return (map == null || map.isEmpty());
    }

    public static boolean isBlank(String value){
        Optional<String> opt = Optional.ofNullable(value);
        return opt.isEmpty() || value.isBlank();
    }

    public static boolean isEmptyList(List<?> list) {
        return list == null || list.isEmpty();
    }

}
