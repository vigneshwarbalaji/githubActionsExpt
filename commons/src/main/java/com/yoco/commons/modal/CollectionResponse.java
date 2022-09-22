package com.yoco.commons.modal;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Collection;

@Data
@NoArgsConstructor
public class CollectionResponse<T> {

    private Collection<T> items;
    private String cursor;

    public static <T> Builder<T> builder() {
        return new Builder<>();
    }

    protected CollectionResponse(Collection<T> items, String cursor) {
        this.items = items;
        this.cursor = cursor;
    }

    public static class Builder<T> {
        private Collection<T> items;
        private String cursor;

        public Builder<T> setItems(Collection<T> items) {
            this.items = items;
            return this;
        }

        public Builder<T> setCursor(String cursor) {
            this.cursor = cursor;
            return this;
        }

        public CollectionResponse<T> build() {
            return new CollectionResponse<>(this.items, this.cursor);
        }
    }
}
