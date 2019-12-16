import React, { useState, useEffect } from 'react';
import { connect } from 'dva';
import moment from 'moment';

import { FormItemInput, FormItemRangePicker, TableWrapper } from '@/components/FormWidget';

import Model from './components/madal';
import { getSettleListBuy } from '../services/services';
import { message } from 'antd';

// 顶部表单查询组件
const MyFormSearch = props => {
  const {
    form: { getFieldDecorator },
    settleStatus,
  } = props;
  const formItemLayout = {};

  return (
    <>
      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="contractNum"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '采购合同号' }}
      />

      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="supplierName"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '供应商' }}
      />
      <FormItemInput
        getFieldDecorator={getFieldDecorator}
        label=""
        fieldId="productName"
        required={false}
        formItemLayout={formItemLayout}
        inputProps={{ placeholder: '产品名称' }}
      />

      <FormItemRangePicker
        getFieldDecorator={getFieldDecorator}
        label={settleStatus === '0' ? '合同签订日期' : '结算时间'}
        fieldId="dateRange"
        required={false}
        datePickerProps={{ format: 'YYYY-MM-DD' }}
        formItemLayout={formItemLayout}
      />
    </>
  );
};

let tableSelectRows = [];
let contractBusinessId = '';
let tableRef = {}; //tableListWraper实例
const Index = props => {
  const { dispatch } = props;
  // 前端默认对 日期列降序排列
  const [sortOrder, setSortOrder] = useState('descend');
  // 当前TAB栏目
  const [tabKey, setTabKey] = useState('0');
  // 弹窗是否可见
  const [confirmModalVisible, setConfirmModalVisible] = useState(false);

  //弹出层选中的table项

  // 这里用于请求 顶部搜索表单中的下拉框数据
  useEffect(() => {
    // 这里在mount时执行一次
  }, []);

  //弹窗显示
  const showModal = row => {
    tableSelectRows = [];
    contractBusinessId = row.contractBusinessId;
    let params = { contractBusinessId, settleStatus: tabKey };
    if (tabKey !== '0') {
      params.settleId = row.settleId;
    } else {
      try {
        _czc1.push(['_trackEvent', '采购结算', '结算', '', '', '']);
      } catch (error) {}
    }
    dispatch({
      type: 'settleModel/getSettleBuyPopupWindow',
      payload: params,
      callback: () => {
        setConfirmModalVisible(true);
      },
    });
  };

  // 表格改变事件， 手动控制排序规则
  const handleTableChange = (pagination, filter, sorter) => {
    if (sortOrder !== sorter.order) {
      setSortOrder(sorter.order);
    }
  };

  const modalOk = values => {
    tableRef.setInitialValue();
    if (tabKey === '1') {
      return setConfirmModalVisible(false);
    }
    if (!tableSelectRows || !tableSelectRows.length > 0) {
      return message.error('请选择车辆');
    }
    dispatch({
      type: 'settleModel/saveSettleBuyPopupWindow',
      payload: { ...values, contractBusinessId, settles: tableSelectRows },
      callback: () => {
        setConfirmModalVisible(false);
        tableRef.setInitialValue();
        //todo reload tableList
      },
    });
  };

  const paramsHandler = _params => {
    let newParams = { ..._params, settleStatus: tabKey };
    if (newParams.dateRange) {
      if (tabKey === '0') {
        newParams.signDateStart = newParams.dateRange.startTime;
        newParams.signDateEnd = newParams.dateRange.endTime;
      } else {
        newParams.settleTimeStart = newParams.dateRange.startTime;
        newParams.settleTimeEnd = newParams.dateRange.endTime;
      }
      delete newParams.dateRange;
    }
    return newParams;
  };
  // tab栏目数据
  const parentTabs = [
    { value: '0', label: '待结算' },
    { value: '1', label: '已结算' },
  ];

  const commonTableProps = {
    parentTabs,
    // 顶部搜索表单中的属性，
    searchFormProps: {},
    // 设置表单默认值， 默认查询条件
    searchParams: {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,
      dateRange: {
        startTime: moment()
          .subtract(30, 'days')
          .format('YYYY-MM-DD'),
        endTime: moment().format('YYYY-MM-DD'),
      },
      contractNum: undefined,
      supplierName: undefined,
      productName: undefined,
    },
    dispatch,
    // 需要请求的model名称
    namespace: 'productStorageModel',
    // 表格改变事件
    handleTableChange,
    // tab栏目改变事件
    onTabChange: activeKey => setTabKey(activeKey),
    //列表请求接口参数处理
    paramsHandler: paramsHandler,
  };

  const columns = [
    {
      title: '序号',
      dataIndex: 'index',
      render: (text, row, index) => index + 1,
    },
    {
      title: '采购合同号',
      dataIndex: 'contractNum',
    },
    {
      title: '供应商',
      dataIndex: 'supplierName',
    },
    {
      title: '产品名称',
      dataIndex: 'productName',
    },

    {
      title: tabKey === '0' ? '未结算车辆数' : '已结算车辆数',
      dataIndex: 'carNum',
    },
    {
      title: tabKey === '0' ? '合同签订日期' : '结算时间',
      dataIndex: tabKey === '0' ? 'signDate' : 'settleTime',
    },
    {
      title: '操作',
      dataIndex: '_option',
      render: (text, row) => (
        <a onClick={() => showModal(row)}>{tabKey === '0' ? '结算' : '查看详细'}</a>
      ),
    },
  ];

  // 当tabkey===0 时的  TableWrapper 组件props
  const CusTomTableProps0 = {
    ...commonTableProps,
    tableProps: {
      hasRowSelection: false,
      columns,
    },
  };

  // 当tabkey===1 时的  TableWrapper 组件props
  const CusTomTableProps1 = {
    ...commonTableProps,
    tableProps: {
      hasRowSelection: false,
      columns,
    },
  };

  // modal弹窗props
  const ComfirmFormModalProps = {
    width: 740,
    visible: confirmModalVisible,
    onOk: values => modalOk(values),
    onCancel: () => setConfirmModalVisible(false),
    title: tabKey === '0' ? '结算' : '结算查看',
    ComProps: {
      settleStatus: tabKey,
      rowKey: 'id',
      onSelectRow: _tableSelectRows => {
        tableSelectRows = _tableSelectRows;
      },
    },
  };
  const myProps = tabKey === '0' ? { ...CusTomTableProps0 } : { ...CusTomTableProps1 };

  return (
    <>
      <TableWrapper
        {...myProps}
        tableWrapperRef={a => {
          tableRef = a;
        }}
        listService={getSettleListBuy}
        currentTabKey={tabKey}
      >
        <MyFormSearch settleStatus={tabKey} />
      </TableWrapper>
      <Model {...ComfirmFormModalProps} />
    </>
  );
};

// 绑定redux
export default connect(state => {
  return {};
})(Index);
