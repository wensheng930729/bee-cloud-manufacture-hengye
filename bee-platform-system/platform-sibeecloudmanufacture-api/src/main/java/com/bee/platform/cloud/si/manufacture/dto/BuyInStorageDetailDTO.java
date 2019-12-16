package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName: BuyInStorageDetailDTO
 * @Description: 采购入库详情实体类
 * @Author: fei.sun
 * @Date: 2019/10/8 10:46
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("采购入库详情实体类")
public class BuyInStorageDetailDTO {

    @ApiModelProperty("样品名称")
    private String productName;

    @ApiModelProperty("车号")
    private String licensePlateNumber;

    @ApiModelProperty("数量")
    private BigDecimal productNumber;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("质检结果")
    private String qualityTestResult;

    @ApiModelProperty("入库时间")
    private String inStorageTime;

    @ApiModelProperty("磅房备注")
    private String remark;

    @ApiModelProperty("化验内容")
    private List<InStorageSampleDTO> inStorageSampleDTOS;

}
