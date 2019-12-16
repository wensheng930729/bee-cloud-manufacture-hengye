package com.bee.platform.common.constants.enums;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 工作台枚举类
 * @Date 2019年5月5日
 */
public class EnumWorkbenchTask {

    /**
     * 工作台任务状态
     */
    public enum TASK_STATUS {

        /**
         * 0 待处理
         */
        todo(0, "待处理"),
        /**
         * 1 已处理
         */
        done(1, "已处理"),
        ;

        TASK_STATUS() {
        }

        TASK_STATUS(Integer key, String value) {
            this.key = key;
            this.value = value;
        }

        private Integer key;
        private String value;

        public Integer getKey() {
            return key;
        }

        public void setKey(Integer key) {
            this.key = key;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }
    }


}
