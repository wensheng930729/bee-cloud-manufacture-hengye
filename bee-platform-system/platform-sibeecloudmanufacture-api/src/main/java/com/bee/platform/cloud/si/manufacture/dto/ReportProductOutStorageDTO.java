package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 成品出库表
 * @Date 2019/10/23 14:06
 */
@Data
@Accessors(chain = true)
@ApiModel("成品出库表返回")
public class ReportProductOutStorageDTO {

        @ApiModelProperty("出库时间")
        private String outStorageTime;

        @ApiModelProperty("规格id")
        private Integer productSpecId;

        @ApiModelProperty("产品")
        private String productName;

        @ApiModelProperty("规格")
        private String productSpecName;

        @ApiModelProperty("仓库")
        private String storageName;

        @ApiModelProperty("出库数量")
        private BigDecimal outStorageNumber;

        @ApiModelProperty("化验项")
        private List<Map<String,Object>> items;


}
