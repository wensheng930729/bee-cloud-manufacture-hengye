package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName SaveSampleProRQ
 * @Description 生产取样RQ
 * @Date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "生产取样RQ")
public class SaveSampleProRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "是否新增0否1是")
    @NotNull(message = "是否新增不能为空")
    private Integer newAdd;

    @ApiModelProperty(value = "矿热炉记录表id")
    private Integer proOreFurnaceSampleId;

    @ApiModelProperty(value = "样品编号")
    @NotBlank(message = "样品编号不能为空")
    private String sampleCode;

    @ApiModelProperty(value = "炉号id")
    @NotNull(message = "炉号id不能为空")
    private Integer furnaceId;

    @ApiModelProperty(value = "炉号名称")
    private String furnaceName;

    @ApiModelProperty(value = "班次:1一班，2二班，3三班")
    @NotNull(message = "班次不能为空")
    private Integer shift;

    @ApiModelProperty(value = "炉次")
    @NotNull(message = "炉次不能为空")
    private Integer furnaceBatch;

//    @ApiModelProperty(value = "出炉日期")
//    @NotBlank(message = "出炉日期不能为空")
//    private String bakedDate;
//
//    @ApiModelProperty(value = "出炉时间")
//    @NotBlank(message = "出炉时间不能为空")
//    private String bakedTime;

    @ApiModelProperty(value = "开班时间")
    @NotNull
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openTime;

    @ApiModelProperty(value = "产品ID")
    @NotNull(message = "产品ID不能为空")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    @NotBlank(message = "产品名称不能为空")
    private String productName;

}
