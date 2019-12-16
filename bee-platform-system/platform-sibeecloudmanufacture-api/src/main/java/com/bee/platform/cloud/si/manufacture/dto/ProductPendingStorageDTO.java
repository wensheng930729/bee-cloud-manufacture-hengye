package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: BuyGoodsPendingStorageDTO
 * @Description: 待入库产品实体类
 * @Author: fei.sun
 * @Date: 2019/9/23 17:40
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("待入库产品实体类")
public class ProductPendingStorageDTO {

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("榜单号")
    private String machineId;

    @ApiModelProperty("产品Id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("产品单位")
    private String productUnit;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("到厂时间(格式：yyyy年MM月dd日 HH:mm:ss)")
    private String arrivalTime;

    @ApiModelProperty("化验结果")
    private Integer analysisResult;

    @ApiModelProperty("处理方式")
    private Integer processMode;

}
