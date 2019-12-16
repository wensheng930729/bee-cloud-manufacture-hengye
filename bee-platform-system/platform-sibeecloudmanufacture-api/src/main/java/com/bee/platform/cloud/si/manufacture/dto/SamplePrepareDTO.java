package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author liang.li
 * @ClassName SamplePrepareDTO
 * @Description 待取样列表DTO
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "待取样列表DTO")
public class SamplePrepareDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "样品业务类型")
    private Integer businessType;

    @ApiModelProperty(value = "样品业务类型名称")
    private String businessTypeName;

    @ApiModelProperty(value = "样品id")
    private Integer productId;

    @ApiModelProperty(value = "样品名称")
    private String productName;

    @ApiModelProperty(value = "推送时间")
    private Date samplePushTime;

    @ApiModelProperty(value = "样品编号列表")
    private List<String> sampleCodeList;

    @ApiModelProperty(value = "采购:磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "采购:车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "采购:完整称重时间")
    private Date weightCompleteTime;
    @ApiModelProperty(value = "采购:称重日期")
    private String weightDate;
    @ApiModelProperty(value = "采购:称重时间")
    private String weightTime;

    @ApiModelProperty(value = "销售:合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "销售:合同编号")
    private String contractNum;

    @ApiModelProperty(value = "生产:矿热炉-取样记录id")
    private Integer proOreFurnaceSampleId;

    @ApiModelProperty(value = "生产:炉号id")
    private Integer furnaceId;

    @ApiModelProperty(value = "生产:炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "生产:班次")
    private Integer shift;

    @ApiModelProperty(value = "生产:炉次")
    private Integer furnaceBatch;

//    @ApiModelProperty(value = "生产:完整出炉时间")
//    private Date bakedCompleteTime;
//    @ApiModelProperty(value = "生产:出炉日期")
//    private String bakedDate;
//    @ApiModelProperty(value = "生产:出炉时间")
//    private String bakedTime;
    @ApiModelProperty(value = "生产:开班时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openTime;
}
