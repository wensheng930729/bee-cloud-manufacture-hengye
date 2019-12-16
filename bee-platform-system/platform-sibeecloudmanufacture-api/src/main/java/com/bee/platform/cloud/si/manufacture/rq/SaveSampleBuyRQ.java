package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName SaveSampleBuyRQ
 * @Description 采购取样RQ
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "采购取样RQ")
public class SaveSampleBuyRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "是否新增0否1是")
    @NotNull(message = "是否新增不能为空")
    private Integer newAdd;

    @ApiModelProperty(value = "合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "样品编号")
    @NotBlank(message = "样品编号不能为空")
    private String sampleCode;

    @ApiModelProperty(value = "产品ID")
    @NotNull(message = "产品ID不能为空")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    @NotBlank(message = "产品名称不能为空")
    private String productName;

    @ApiModelProperty(value = "称重日期")
    private String weightDate;

    @ApiModelProperty(value = "称重时间")
    private String weightTime;

    @ApiModelProperty(value = "磅单业务id")
    @NotNull(message = "磅单业务id不能为空")
    private List<String> machineIdList;

}
