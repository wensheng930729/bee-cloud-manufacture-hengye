package com.bee.platform.cloud.si.manufacture.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.cloud.si.manufacture.entity.RevisionStorageDetail;

import java.util.List;

/**
 * @ClassName: RevisionStorageDetailService
 * @Description: 盘库记录
 * @Author: fei.sun
 * @Date: 2019/10/31 11:24
 * @Version: 1.0
 */
public interface RevisionStorageDetailService extends IService<RevisionStorageDetail> {
    /**
     * 批量添加盘库记录
     * @param revisionStorageDetails a
     * @return b
     */
    boolean insertRevisionStorageDetails(List<RevisionStorageDetail> revisionStorageDetails);
}
