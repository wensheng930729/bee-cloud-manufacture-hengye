package com.bee.platform.common.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;


/**
 * @ClassName Page
 * @Description 分页对象
 * @author zhigang.zhou
 * @Date 2018年10月26日 下午1:16:35
 * @version 1.0.0
 */
@Data
@AllArgsConstructor
@ApiModel("新的分页对象")
public class Page implements Serializable {

    private static final long serialVersionUID = -8820698080806531301L;

    @ApiModelProperty("分页条数，默认5条")
    private Integer pageSize = 5;

    @ApiModelProperty("当前页数, 默认第一页")
    private Integer currentPage = 1;

    @ApiModelProperty(value = "总页数", hidden = true)
    private Integer totalPage;

    @ApiModelProperty(value = "总条数", hidden = true)
    private Integer totalRecords;

    @ApiModelProperty(value = "排序规则，字段名.aes或者desc，示例：field_one.aes,field_two.desc")
    private String orderStage;

    @ApiModelProperty("是否查询总条数（默认true)，在不需要显示总条数和总页数的设置为false")
    private Boolean searchCount = true;

    public Page() {

    }

    public Page(Integer pageSize, Integer currentPage) {
        this.pageSize = pageSize;
        this.currentPage = currentPage;
    }

    public Page(Integer pageSize, Integer currentPage, String orderStage) {
        this.pageSize = pageSize;
        this.currentPage = currentPage;
        this.orderStage = orderStage;
    }

    public void addSort(String sortString) {
        this.orderStage = sortString;
    }
}
