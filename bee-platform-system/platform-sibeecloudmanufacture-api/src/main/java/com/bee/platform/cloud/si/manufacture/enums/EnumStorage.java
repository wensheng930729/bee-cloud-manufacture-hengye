package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @ClassName: EnumStorage
 * @Description: 仓储相关枚举
 * @Author: fei.sun
 * @Date: 2019/9/24 11:14
 * @Version: 1.0
 */
public class EnumStorage {

    @Getter
    @AllArgsConstructor
    public enum Status{
        /**
         * 使用中
         */
        normal(1,"正常"),
        /**
         * 已删除
         */
        not_normal(0,"删除");
        private Integer key;
        private String value;
    }

    @Getter
    @AllArgsConstructor
    public enum PutStorage{
        /**
         * 已入库
         */
        storage(1,"已入库"),
        /**
         * 待入库
         */
        not_storage(0,"待入库");
        private Integer key;
        private String value;
    }

    @Getter
    @AllArgsConstructor
    public enum AnalysisResult{
        /**
         * 化验结果不合格
         */
        unqualified(0,"不合格"),
        /**
         * 化验结果合格
         */
        qualified(1,"合格");
        private Integer key;
        private String value;
    }

    @Getter
    @AllArgsConstructor
    public enum ProcessMode{
        /**
         * 确认入库
         */
        confirm_storage(1,"确认入库"),
        /**
         * 折价入库
         */
        discount_storage(0,"折价入库");
        private Integer key;
        private String value;
    }

    @Getter
    @AllArgsConstructor
    public enum OutStorageStatus{
        /**
         * 已出库
         */
        out_storage(1,"已出库"),
        /**
         * 待出库
         */

        wait_storage(0,"待出库");

        private Integer key;
        private String value;
    }
}
