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
 * @Description 销售取样RQ
 * @Date 2019/9/26
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "销售取样RQ")
public class SaveSampleSaleRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "是否新增0否1是")
    @NotNull(message = "是否新增不能为空")
    private Integer newAdd;

    @ApiModelProperty(value = "合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "合同号")
    @NotBlank(message = "合同号不能为空")
    private String contractNum;

    @ApiModelProperty(value = "样品编号")
    @NotBlank(message = "样品编号不能为空")
    private String sampleCode;

    @ApiModelProperty(value = "产品ID")
    @NotNull(message = "产品ID不能为空")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    @NotBlank(message = "产品名称不能为空")
    private String productName;

    @ApiModelProperty(value = "吨袋编码")
    @NotNull(message = "吨袋编码不能为空")
    private List<String> tonCodeList;

}
