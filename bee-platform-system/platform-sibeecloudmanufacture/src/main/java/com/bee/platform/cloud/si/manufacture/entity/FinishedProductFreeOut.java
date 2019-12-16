package com.bee.platform.cloud.si.manufacture.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @ClassName: FinishedProductFreeOut
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/11/15 16:50
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@TableName("finished_product_free_out")
public class FinishedProductFreeOut extends Model<FinishedProductFreeOut> {

    @TableId(type = IdType.AUTO)
    private Integer id;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 当前登录的企业id
     */
    private Integer orgId;
    /**
     * 关联出库车俩业务id
     */
    private String contractCarId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 产品规格
     */
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    private String productSpecName;
    /**
     * 出库数量
     */
    private BigDecimal productNumber;
    /**
     * 仓库id
     */
    private Integer storageId;
    /**
     * 仓库名称
     */
    private String storageName;
    /**
     * 数据状态（数据状态0删除1正常）
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 创建时间
     */
    private LocalDateTime createTime;
    /**
     * 修改人
     */
    private Integer modifyId;
    /**
     * 修改人
     */
    private String modifier;
    /**
     * 修改时间
     */
    private LocalDateTime modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
