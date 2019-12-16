package com.bee.platform.cloud.si.manufacture.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * @author liang.li
 * @ClassName EnumSampleRelation
 * @Description TODO
 * @Date
 */
public class EnumSampleRelation {

    /**
     * 样品化验状态枚举
     */
    @Getter
    @AllArgsConstructor
    public enum SampleAssayStatus {
        ABANDON(0, "已弃用"),
        PREPARE_ASSAY(1, "待化验"),
        ASSAYING(2, "化验中"),
        ALREADY_ASSAY(3, "已化验"),

        ;
        private Integer key;
        private String value;
    }

    /**
     * 样品化验状态枚举
     */
    @Getter
    @AllArgsConstructor
    public enum SampleStatus {
        NOT_YET(0, "未取样"),
        COMPLETED(1, "完成取样"),
        ;
        private Integer key;
        private String value;
    }

    /**
     * 样品化验状态枚举
     */
    @Getter
    @AllArgsConstructor
    public enum SampleCodeType {
        SAMPLE(1, "样品编码"),
        TON(2, "吨袋编码"),
        ;
        private Integer key;
        private String value;
    }

    /**
     * 样品推送状态枚举
     */
    @Getter
    @AllArgsConstructor
    public enum SamplePushStatus {
        NOT_YET(0, "未推送"),
        ALREADY(1, "已推送"),
        ;
        private Integer key;
        private String value;
    }

    /**
     * 质检主任相关枚举
     */
    @Getter
    @AllArgsConstructor
    public enum SampleAssayResultType {
        IN(0, "输入项"),
        OUT(1, "输出项");
        private Integer key;
        private String value;
    }

    /**
     * 样品化验结果业务类型
     */
    @Getter
    @AllArgsConstructor
    public enum SampleAssayResultBusinessType {
        PURCHASE(1, "采购"),
        SALE(2, "销售"),
        PRODUCE(3, "生产");
        private Integer key;
        private String value;
    }

    /**
     * 质检主任相关枚举
     */
    @Getter
    @AllArgsConstructor
    public enum QualityAssayResult {
        NO(0, "不合格"),
        YES(1, "合格");
        private Integer key;
        private String value;
    }

    /**
     * 质检主任是否确认磅单车辆样品车辆
     */
    @Getter
    @AllArgsConstructor
    public enum QualityExamStatus {
        NO(0, "未确认"),
        YES(1, "已确认");
        private Integer key;
        private String value;
    }

    /**
     * 质检主任是否已出质检单
     */
    @Getter
    @AllArgsConstructor
    public enum QualityAssayStatus {
        NOT(0, "未出"),
        ALREADY(1, "已出");
        private Integer key;
        private String value;
    }

    /**
     * 化验结果
     */
    @Getter
    @AllArgsConstructor
    public enum SampleResults {
        NO(0, "未化验"),
        YES(1, "已化验");
        private Integer key;
        private String value;
    }

    /**
     * 不合格车辆列表入库确认信息（0-待确认1-已确认)
     */
    @Getter
    @AllArgsConstructor
    public enum InStorageConfirmType {
        NO(0, "待确认"),
        YES(1, "已确认");
        private Integer key;
        private String value;
    }

    /**
     * 样品化验状态枚举
     */
    @Getter
    @AllArgsConstructor
    public enum ProductUnit {
        PERCENTAGE(0, "%"),
        EXTREME_RATIO(1, "‱"),
        ;
        private Integer key;
        private String value;
    }

    /**
     * 样品化验结果业务类型
     */
    @Getter
    @AllArgsConstructor
    public enum CodeScanSampleType {
        PURCHASE(1, "采购"),
        SALE(2, "销售"),
        PRODUCE(3, "生产"),
        BAG(4, "成品装袋");
        private Integer key;
        private String value;
    }
}
