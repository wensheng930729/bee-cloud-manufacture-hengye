package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author liang.li
 * @ClassName SampleAlreadyDTO
 * @Description 已取样列表DTO
 * @Date 2019/9/27
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "已取样列表DTO")
public class SampleAlreadyDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "样品业务类型")
    private Integer businessType;
    @ApiModelProperty(value = "样品业务类型名称")
    private String businessTypeName;
    @ApiModelProperty(value = "样品编号")
    private String sampleCode;
    @ApiModelProperty(value = "样品id")
    private Integer productId;
    @ApiModelProperty(value = "样品名称")
    private String productName;
    @ApiModelProperty(value = "取样时间")
    private Date sampleTime;
    @ApiModelProperty(value = "取样人")
    private String samplePerson;
    @ApiModelProperty(value = "采购:车牌号")
    private String trainNumber;
    @ApiModelProperty(value = "采购:磅单号")
    private String machineId;
    @ApiModelProperty(value = "销售:合同业务id")
    private String contractBusinessId;
    @ApiModelProperty(value = "销售:合同编号")
    private String contractNum;
    @ApiModelProperty(value = "销售:吨袋号列表")
    private List<String> tonCodeList;
    @ApiModelProperty(value = "生产:炉号id")
    private Integer furnaceId;
    @ApiModelProperty(value = "生产:炉号名称")
    private String furnaceName;
    @ApiModelProperty(value = "生产:班次")
    private Integer shift;
    @ApiModelProperty(value = "生产:出炉批次")
    private String furnaceBatch;

}
