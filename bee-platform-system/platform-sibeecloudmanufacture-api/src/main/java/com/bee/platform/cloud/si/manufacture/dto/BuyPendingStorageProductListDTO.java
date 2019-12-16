package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: PendingStorageProductListDTO
 * @Description: 采购待入库列表实体类
 * @Author: fei.sun
 * @Date: 2019/9/24 14:28
 * @Version: 1.0
 */
@Data
@ApiModel("采购待入库列表实体类")
@Accessors(chain = true)
public class BuyPendingStorageProductListDTO {

    @ApiModelProperty("待入库产品业务Id")
    private String buyProductPendingStorageId;

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("产品Id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("产品单位")
    private String productUnit;

    @ApiModelProperty("产品规格id")
    private Integer productSpecId;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("到厂时间")
    private String analysisTime;

    @ApiModelProperty("化验结果code")
    private Integer analysisResult;

    @ApiModelProperty("化验结果名")
    private String analysisResultName;

    @ApiModelProperty("处理方式code")
    private Integer processMode;

    @ApiModelProperty("处理方式名")
    private String processModeName;

    @ApiModelProperty("磅房备注")
    private String remark;
}
