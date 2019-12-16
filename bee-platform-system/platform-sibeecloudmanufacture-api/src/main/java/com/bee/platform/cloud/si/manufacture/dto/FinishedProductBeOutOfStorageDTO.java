package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: FinishedProductBeOutOfStorageDTO
 * @Description:  保存产成品待出库信息
 * @Author: fei.sun
 * @Date: 2019/9/25 19:22
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("保存产成品待出库信息")
public class FinishedProductBeOutOfStorageDTO {

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("承运商id")
    private Long carrierId;

    @ApiModelProperty("承运商")
    private String carrierName;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("货物id")
    private Integer productId;

    @ApiModelProperty("样品名称")
    private String productName;

    @ApiModelProperty("收货重量")
    private BigDecimal productNumber;

    @ApiModelProperty("货物数量单位")
    private String productUnit;

    @ApiModelProperty("司机名称")
    private String driverName;

    @ApiModelProperty("联系方式")
    private String contact;

    @ApiModelProperty("磅单id")
    private String  machineId;

}
