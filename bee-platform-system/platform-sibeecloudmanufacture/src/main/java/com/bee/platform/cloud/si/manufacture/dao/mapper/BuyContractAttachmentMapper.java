package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.entity.BuyContractAttachment;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 采购合同附近信息 Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-09-23
 */
public interface BuyContractAttachmentMapper extends BaseMapper<BuyContractAttachment> {

    /**
     * 合同附件新增
     * @param files
     * @return
     */
    Integer insertAttachments(@Param("data")List<BuyContractAttachment> files);
}
