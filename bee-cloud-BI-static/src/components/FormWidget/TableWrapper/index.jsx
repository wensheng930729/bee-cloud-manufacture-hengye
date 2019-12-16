import { Button, Card, Form, Tabs, message } from 'antd';
import React, { Component } from 'react';
import { isEqual, isFunction } from 'lodash';
import PropTypes from 'prop-types';
import { connect } from 'dva';

import StandardTable from '../StandardTable';

import styles from './index.less';

const { TabPane } = Tabs;

@Form.create()
@connect(({ listTableModel, loading }) => ({
  data: listTableModel.data,
  loading: loading.effects['listTableModel/getListEffect'],
}))
class TableList extends Component {
  static propTypes = {
    // 表格props
    tableProps: PropTypes.shape({
      columns: PropTypes.array.isRequired,
    }).isRequired,
    // 默认查询条件
    searchParams: PropTypes.object.isRequired,
    // 表格改变事件
    handleTableChange: PropTypes.func,
    handleMenuClick: PropTypes.func,
    // 具有 "添加/新增" 的按钮事件
    onAddClick: PropTypes.func,
    sortOrder: PropTypes.string,
    // tab栏目  [{value:'',label:''}]
    parentTabs: PropTypes.array,

    handleSelectRows: PropTypes.func,

    // 一个函数， 返回list接口的参数
    paramsHandler: PropTypes.func,
    // list接口
    listService: PropTypes.func.isRequired,
    // 一个函数，返回格式为  {list:[],pagination: {}}
    dataHandler: PropTypes.func,
    // 在Alert中是否显示总计
    alertProps: PropTypes.object,
  };

  static defaultProps = {
    handleTableChange: undefined,
    handleMenuClick: undefined,
    onAddClick: undefined,
    sortOrder: 'descend',
    parentTabs: [],
    handleSelectRows: undefined,
    alertProps: {
      hasAlert: false,
    },

    paramsHandler: undefined,
    dataHandler: undefined,
  };

  constructor(props) {
    super(props);
    this.state = {
      selectedRows: [],
      searchParams: props.searchParams,

      currentTabKey: props.parentTabs.length > 0 ? props.parentTabs[0].value : '',
    };

    this.tableWrapperRef = React.createRef();
  }

  componentDidMount() {
    this.setInitialValue();
    const { tableWrapperRef } = this.props;
    if (isFunction(tableWrapperRef) && this.tableWrapperRef && this.tableWrapperRef.current) {
      tableWrapperRef(this);
    }
  }

  // React声明周期方法，在组件更新结束之后执行，在初始化render时不执行
  componentDidUpdate(prevProps) {
    // 得到从contractStatement获取到的tab值
    const { currentTabKey } = this.props;
    if (currentTabKey && !isEqual(prevProps.currentTabKey, currentTabKey)) {
      this.setInitialValue();
    }
  }

  componentWillUnmount() {
    const { dispatch } = this.props;

    dispatch({
      type: 'listTableModel/getListReduce',
      payload: { list: [], pagination: {} },
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  }

  setInitialValue = () => {
    const { form, searchParams } = this.props;
    this.getTableData(searchParams);

    form.setFieldsValue(searchParams);
  };

  handleStandardTableChange = (pagination, filtersArg, sorter) => {
    const { current, pageSize } = pagination;
    const { searchParams } = this.state;

    const { handleTableChange, sortOrder } = this.props;

    if (handleTableChange) {
      handleTableChange(pagination, filtersArg, sorter);
    }

    if (
      (searchParams.currentPage === current && searchParams.pageSize === pageSize) ||
      (sorter.order && sortOrder && sorter.order !== sortOrder)
    ) {
      return;
    }

    this.getTableData({ currentPage: current, pageSize });
  };

  handleMenuClick = e => {
    const { handleMenuClick } = this.props;
    const { selectedRows } = this.state;
    if (!selectedRows) return;

    if (handleMenuClick) {
      handleMenuClick(e.key, selectedRows);
    }
    this.setState({
      selectedRows: [],
    });
  };

  handleSelectRows = (selectedRowKeys, selectedRows) => {
    this.setState({
      selectedRows,
    });
    const { handleSelectRows } = this.props;
    if (handleSelectRows) {
      handleSelectRows(selectedRowKeys, selectedRows);
    }
  };

  handleSearch = e => {
    e.preventDefault();
    const { form } = this.props;
    form.validateFields((err, fieldsValue) => {
      if (err) return;

      this.getTableData({ ...fieldsValue, currentPage: 1, pageSize: 10 });
    });
  };

  getTableData = payload => {
    const { dispatch, paramsHandler, listService, dataHandler } = this.props;
    if (!listService) throw new Error('listService is undefined');

    const { searchParams } = this.state;

    let requestParams = {};

    if (isFunction(paramsHandler)) {
      requestParams = paramsHandler({ ...searchParams, ...payload });
    } else {
      // 旧日期
      const { dateRange, ...rest } = searchParams;
      let oldstartTime = '';
      let oldendTime = '';

      if (dateRange) {
        oldstartTime = dateRange.startTime;
        oldendTime = dateRange.endTime;
      }

      const { dateRange: newdateRange, ...other } = payload;

      requestParams = {
        startTime: newdateRange ? newdateRange.startTime : oldstartTime,
        endTime: newdateRange ? newdateRange.endTime : oldendTime,
        ...rest,
        ...other,
      };
    }

    const newsearchParams = { ...searchParams, ...payload };
    this.setState({
      searchParams: newsearchParams,
    });
    dispatch({
      type: 'listTableModel/getListEffect',
      payload: requestParams,
      listService,
      dataHandler,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  handleTabChange = key => {
    this.setState({ currentTabKey: key });
    const { onTabChange } = this.props;
    if (onTabChange) {
      onTabChange(key);
    }
  };

  renderSimpleForm() {
    const { form, searchFormProps, children } = this.props;

    const selfProps = { form, ...searchFormProps };
    return (
      <Form onSubmit={this.handleSearch} layout="inline">
        {React.cloneElement(children, { ...selfProps })}
        <Form.Item>
          <Button type="primary" htmlType="submit">
            查询
          </Button>
        </Form.Item>
      </Form>
    );
  }

  renderTable = () => {
    const {
      tableProps,
      onAddClick,
      operateText,
      onOperateClick,
      data,
      loading,
      alertProps,
    } = this.props;

    const newTableProps = {
      ...tableProps,
      data,
      loading,
      alertProps,
    };
    return (
      <div className={styles.tableList}>
        {this.renderSimpleForm()}
        <div className={styles.tableListOperator}>
          {onAddClick ? (
            <Button icon="plus" type="primary" onClick={onAddClick}>
              新建
            </Button>
          ) : null}

          {operateText && onOperateClick ? (
            <Button type="primary" onClick={onOperateClick}>
              {operateText}
            </Button>
          ) : null}

          {/* {selectedRows.length > 0 && (
          <Dropdown overlay={menu}>
            <Button>
              批量操作 <Icon type="down" />
            </Button>
          </Dropdown>
        )} */}
        </div>
        <StandardTable
          {...newTableProps}
          onSelectRow={this.handleSelectRows}
          onChange={this.handleStandardTableChange}
        />
      </div>
    );
  };

  renderTabs = () => {
    const { parentTabs } = this.props;
    const { currentTabKey } = this.state;

    if (Array.isArray(parentTabs) && parentTabs.length > 0) {
      const tabs = parentTabs.map(item => (
        <TabPane tab={item.label} key={item.value}>
          {this.renderTable()}
        </TabPane>
      ));
      return (
        <Tabs activeKey={currentTabKey} onChange={this.handleTabChange}>
          {tabs}
        </Tabs>
      );
    }
    return this.renderTable();
  };

  render() {
    return (
      <Card bordered={false} ref={this.tableWrapperRef}>
        {this.renderTabs()}
      </Card>
    );
  }
}

export default TableList;
